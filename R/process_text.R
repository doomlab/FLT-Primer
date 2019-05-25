# Data --------------------------------------------------------------

## Setting working directory to current folder using RStudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Importing the data in the provided data folder
master <- read.csv("../data/tidy_words.csv", stringsAsFactors = F)

## Show data structure
## Data is in tidy format with concept in the word column
## Participant answer in the answer column
head(master)

# Spelling  ---------------------------------------------------------------

## Install the hunspell package if necessary
#install.packages("hunspell")

library(hunspell)

## Check the participant answers
## The output is a list of spelling errors for each line
spelling_errors <- hunspell(master$answer)

## Check for suggestions
spelling_suggest <- lapply(spelling_errors, hunspell_suggest)

## Replace with most likely suggestion
spell_checked <- master

### Loop over the data.frame
for (i in 1:nrow(spell_checked)){
  ### See if there are spelling errors
  if (length(spelling_errors[[i]]) > 0) {
    ### Loop over all errors
    for (q in 1:length(spelling_errors[[i]])){
      ### Replace with the first answer
      spell_checked$answer[i] <- gsub(spelling_errors[[i]][q], 
                                      spelling_suggest[[i]][[q]][1],
                                      spell_checked$answer[i])
    }
  }
}

# Lemmatization -----------------------------------------------------------

lemmas <- spell_checked

## Install the koRpus package
#install.packages("koRpus")
#install.packages("koRpus.lang.en")
#install.packages("tokenizers")

## You must load both packages separately
library(koRpus)
library(koRpus.lang.en)
library(tokenizers)

## Install TreeTagger 
#https://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/

## Find all types for faster lookup
all_answers <- paste0(lemmas$answer, collapse = " ")
all_answers <- unlist(tokenize_words(all_answers))
all_answers <- unique(all_answers)

## This function has both suppressWarnings & suppressMessages
## You should first view these to ensure proper processing
temp_tag <- suppressWarnings(
  suppressMessages(
    ## Note: the NULL option is to control for the <unknown> that appears
    ## to occur with the last word in each text
    treetag(c(all_answers, "NULL"), 
            ## Control the parameters of treetagger
            treetagger="manual", format="obj",
            TT.tknz=FALSE, lang="en",
            TT.options=list(path="~/TreeTagger", preset="en"))))

## Remove all tags not using
replacement_lemmas <- temp_tag@TT.res
replacement_lemmas <- subset(replacement_lemmas, 
                             #unknown values
                             lemma != "<unknown>" & 
                             #numbers
                             lemma!= "@card@" & 
                             #token should change more than case
                             token != tolower(lemma)) 

## Install the stringi package
#install.packages("stringi")

## Replace all the original tokens with new lemmas
lemmas$answer <- stri_replace_all_regex(lemmas$answer, 
                       pattern = paste("\\b", replacement_lemmas$token, "\\b", sep = ""),
                       replacement = replacement_lemmas$lemma,
                       F, list(case_insensitive = TRUE))

# Multi-Word Strings -----------------------------------------------------

multi_words <- data.frame(Word=character(),
                          Feature=character(), 
                          Frequency=numeric(), 
                          stringsAsFactors=FALSE)

unique_concepts <- unique(lemmas$word)

## Install dplyr
#install.packages("dplyr")

library(dplyr)

## Loop over each word
for (i in 1:length(unique_concepts)){
  
  ## Create parts of speech for clustering together
  temp_tag <- suppressWarnings(
    suppressMessages(
      treetag(c(lemmas$answer[lemmas$word  == unique_concepts[i]], "NULL"), 
              ## Control the parameters of treetagger
              treetagger="manual", format="obj",
              TT.tknz=FALSE, lang="en",
              TT.options=list(path="~/TreeTagger", preset="en"))))
  
  ## Save only the data.frame, remove NULL
  temp_tag <- temp_tag@TT.res[-nrow(temp_tag@TT.res) , ]
  
  ## Subset out information you don't need
  temp_tag <- subset(temp_tag, 
                     wclass != "comma" & wclass != "determiner" & 
                       wclass != "preposition" & wclass != "modal" &
                       wclass != "predeterminer" & wclass != "particle" &
                       wclass != "to" & wclass != "punctuation" & 
                       wclass != "fullstop" & wclass != "conjunction" & 
                       wclass != "pronoun")
  
  ## Create a temporary tibble 
  temp_tag_tibble <- as_tibble(temp_tag)
  ## Create part of speech and features combined
  temp_tag_tibble <- mutate(temp_tag_tibble, 
                            two_words = paste(token, lead(token), sep = "_"))
  temp_tag_tibble <- mutate(temp_tag_tibble, 
                            three_words = paste(token, lead(token), lead(token, n = 2L), sep = "_"))
  temp_tag_tibble <- mutate(temp_tag_tibble, 
                            two_words_pos = paste(wclass, lead(wclass), sep = "_"))
  temp_tag_tibble <- mutate(temp_tag_tibble, 
                            three_words_pos = paste(wclass, lead(wclass), lead(wclass, n = 2L), sep = "_"))
  
  ## Find verb noun or verb adjective nouns to cluster on 
  verb_nouns <- grep("\\bverb_noun", temp_tag_tibble$two_words_pos)
  verb_adj_nouns <- grep("\\bverb_adjective_noun", temp_tag_tibble$three_words_pos)
  
  ## Use combined and left over features
  features_for_table <- c(temp_tag_tibble$two_words[verb_nouns], 
                          temp_tag_tibble$three_words[verb_adj_nouns],
                          temp_tag_tibble$token[-c(verb_nouns, verb_nouns+1, 
                                                   verb_adj_nouns, verb_adj_nouns+1, verb_adj_nouns+2)])
  
  ## Create a table of frequencies
  word_table <- as.data.frame(table(features_for_table))
  
  ## Clean up the table
  word_table$Word <- unique_concepts[i]
  colnames(word_table) = c("Feature", "Frequency", "Word")
  
  multi_words <- rbind(multi_words, word_table[ , c(3, 1, 2)])
  
}

# Stopwords and Other Exclusions ------------------------------------------

no_stop <- lemmas

## Install the stopwords package or use tm
#install.packages("stopwords")

## Exclude all stopwords
no_stop$answer <- stri_replace_all_regex(no_stop$answer, 
                                        pattern = paste(stopwords(language = "en", 
                                                                  source = "snowball"), 
                                                        collapse = "\\b|\\b"),
                                        replacement = "",
                                        F, list(case_insensitive = TRUE))

## Remove stop words
multi_words <- subset(multi_words, 
                      !(Feature %in% stopwords(language = "en", source = "snowball")))

# Bag of Words ------------------------------------------------------------

bag_words <- data.frame(Word=character(),
                        Feature=character(), 
                        Frequency=numeric(), 
                        stringsAsFactors=FALSE) 



## Loop over each word
for (i in 1:length(unique_concepts)){

  ## Create a table of frequencies
  word_table <- as.data.frame(table(
    unlist(
      ## Tokenize the words
      tokenize_words(
        ## Put all answers together in one character string
        paste0(no_stop$answer[no_stop$word == unique_concepts[i]], collapse = " ")))))
  
  ## Clean up the table
  word_table$Word <- unique_concepts[i]
  colnames(word_table) = c("Feature", "Frequency", "Word")
  
  bag_words <- rbind(bag_words, word_table[ , c(3, 1, 2)])
  
}

# Examine against previous work --------------------------------------------

library(readxl)
b_data <- read.csv("../data/final words 2017.csv", stringsAsFactors = F)

## Reduce down only to overlap words
b_data <- subset(b_data, cue %in% unique_concepts)
b_data <- b_data[ , c("where", "cue", "feature", "translated",
                      "frequency_feature", "frequency_translated") ]

library(lsa)

cosine_values <- data.frame(word=unique_concepts,
                            raw_b=1:length(unique_concepts), 
                            raw_m=1:length(unique_concepts),
                            raw_v=1:length(unique_concepts),
                            translated_b=1:length(unique_concepts), 
                            translated_m=1:length(unique_concepts), 
                            translated_v=1:length(unique_concepts), 
                            stringsAsFactors=FALSE)

for (i in 1:length(unique_concepts)){
 
  temp_b <- b_data[b_data$cue == unique_concepts[i] & b_data$where == "b", ]
  temp_m <- b_data[b_data$cue == unique_concepts[i] & b_data$where == "m", ] 
  temp_v <- b_data[b_data$cue == unique_concepts[i] & b_data$where == "v", ]
  temp_bag <- bag_words[bag_words$Word == unique_concepts[i], ]
  colnames(temp_bag)[2] = "feature"
  
  ## Based on unprocessed data 
  temp_merge_b <- merge(temp_b, temp_bag, by = "feature", all = T)
  temp_merge_m <- merge(temp_m, temp_bag, by = "feature", all = T)
  temp_merge_v <- merge(temp_v, temp_bag, by = "feature", all = T)
  
  temp_merge_b[c("frequency_feature", "Frequency")][is.na(temp_merge_b[c("frequency_feature", "Frequency")])] <- 0
  temp_merge_m[c("frequency_feature", "Frequency")][is.na(temp_merge_m[c("frequency_feature", "Frequency")])] <- 0
  temp_merge_v[c("frequency_feature", "Frequency")][is.na(temp_merge_v[c("frequency_feature", "Frequency")])] <- 0
  
  cosine_values$raw_b[i] <- cosine(temp_merge_b$frequency_feature, temp_merge_b$Frequency)
  cosine_values$raw_m[i] <- cosine(temp_merge_m$frequency_feature, temp_merge_m$Frequency)
  cosine_values$raw_v[i] <- cosine(temp_merge_v$frequency_feature, temp_merge_v$Frequency)
  
  ## Based on processed data
  colnames(temp_bag)[2] = "translated"
  temp_merge_b <- merge(temp_b, temp_bag, by = "translated", all = T)
  temp_merge_m <- merge(temp_m, temp_bag, by = "translated", all = T)
  temp_merge_v <- merge(temp_v, temp_bag, by = "translated", all = T)
  
  temp_merge_b[c("frequency_translated", "Frequency")][is.na(temp_merge_b[c("frequency_translated", "Frequency")])] <- 0
  temp_merge_m[c("frequency_translated", "Frequency")][is.na(temp_merge_m[c("frequency_translated", "Frequency")])] <- 0
  temp_merge_v[c("frequency_translated", "Frequency")][is.na(temp_merge_v[c("frequency_translated", "Frequency")])] <- 0
  
  ### This process creates duplicates
  temp_merge_b <- temp_merge_b[!duplicated(temp_merge_b$translated), ]
  temp_merge_m <- temp_merge_m[!duplicated(temp_merge_m$translated), ]
  temp_merge_v <- temp_merge_v[!duplicated(temp_merge_v$translated), ]
  
  cosine_values$translated_b[i] <- cosine(temp_merge_b$frequency_translated, temp_merge_b$Frequency)
  cosine_values$translated_m[i] <- cosine(temp_merge_m$frequency_translated, temp_merge_m$Frequency)
  cosine_values$translated_v[i] <- cosine(temp_merge_v$frequency_translated, temp_merge_v$Frequency)
  
  }

cosine_values$raw_m[is.nan(cosine_values$raw_m)] <- NA
cosine_values$raw_v[is.nan(cosine_values$raw_v)] <- NA
cosine_values$translated_m[is.nan(cosine_values$translated_m)] <- NA
cosine_values$translated_v[is.nan(cosine_values$translated_v)] <- NA

apply(cosine_values[ , -1], 2, mean, na.rm = T)


# Comparison to MEN -------------------------------------------------------

men_data <- read.table("../data/MEN_dataset_natural_form_full.txt",
                       quote="\"", comment.char="", stringsAsFactors=FALSE)

library(tidyr)
bag_words_spread <- spread(bag_words, key = Word, value = Frequency, fill = 0)
bag_words_cosine <- cosine(as.matrix(bag_words_spread[ , -1]))
bag_words_cosine <- as_tibble(bag_words_cosine)
bag_words_cosine$cue <- colnames(bag_words_cosine)
bag_words_cosine <- gather(bag_words_cosine, key = target, 
                           value = cosine, airplane:zebra)
bag_words_cosine$key <- paste(bag_words_cosine$cue, bag_words_cosine$target, sep = " ")
men_data$key <- paste(men_data$V1, men_data$V2)
colnames(men_data) <- c("cue", "target", "rating", "key")
  
men_merge = merge(men_data, bag_words_cosine, by = "key")


# Clustering --------------------------------------------------------------

library(cluster)

bag_words_dist <- dist(bag_words_spread[ , -1], method = "euclidean")
bag_words_cluster <- hclust(bag_words_dist, method = "ward.D2")
#plot(bag_words_cluster, hang = -1)

sapply(2:34, #34 in wu_barsalou 
       function(x) summary(
         silhouette(cutree(bag_words_cluster, k = x),
                    bag_words_dist))$avg.width #find the widths
)

#probaby 6ish clusters can go more defined
bag_words_category <- as.data.frame(cutree(bag_words_cluster, k = 6))
bag_words_category$feature <- bag_words_spread$Feature
colnames(bag_words_category)[1] <- "category"
bag_words_category$totals <- rowSums(bag_words_spread[ , -1])
table(bag_words_category$category)
hist(bag_words_category$totals[bag_words_category$category == 1], breaks = 40)
tapply(bag_words_category$totals, bag_words_category$category, mean)
tapply(bag_words_category$totals, bag_words_category$category, sd)
tapply(bag_words_category$totals, bag_words_category$category, min)
tapply(bag_words_category$totals, bag_words_category$category, max)


# Clustering that I tried -------------------------------------------------

category_combos <- list()
bag_words_reduce <- subset(bag_words, Percent >= 10)

for (i in 1:5){
  
  bag_words_spread <- spread(bag_words_reduce[ , -c(4,5)], key = Word, value = Frequency, fill = 0)
  bag_words_dist <- dist(bag_words_spread[ , -1], method = "euclidean")
  bag_words_cluster <- hclust(bag_words_dist, method = "ward.D2")
  bag_words_category <- as.data.frame(cutree(bag_words_cluster, k = 20))
  bag_words_category$feature <- bag_words_spread$Feature
  colnames(bag_words_category)[1] <- "category"
  #bag_words_category$totals <- rowSums(bag_words_spread[ , -1])
  cat_temp <- as.data.frame(table(bag_words_category$category))
  keep <- cat_temp$Var1[cat_temp$Freq >= 10 & cat_temp$Freq<500]
  category_combos[[i]] <- subset(bag_words_category, category %in% keep)
  
  #remove words and start over
  bag_words_reduce <- bag_words_reduce[!(bag_words_reduce$Feature %in% unique(category_combos[[i]]$feature)) , ]
  bag_words_spread <- spread(bag_words_reduce[ , -c(4,5)], key = Word, value = Frequency, fill = 0)
  
}


## kmeans

library(cluster)
library(factoextra)

bag_words_reduce <- subset(bag_words, Percent >= 20)
bag_words_spread <- spread(bag_words_reduce[ , -c(4,5)], 
                           key = Word, value = Frequency, fill = 0)
bag_words_cluster <- kmeans(bag_words_spread[ , -1], 
                            centers = 10,
                            nstart = 25)
#bag_words_dist <- get_dist(bag_words_spread[ , -1])
#fviz_dist(bag_words_dist)  

fviz_cluster(bag_words_cluster, data = bag_words_spread[ , -1]) 

fviz_nbclust(bag_words_spread[ , -1], kmeans, method = "wss")
fviz_nbclust(bag_words_spread[ , -1], kmeans, method = "silhouette")

##scaling and just looking at various dimensions of the data
library(factoextra)
bag_words_reduce <- subset(bag_words, Percent >= 60)
bag_words_spread <- spread(bag_words_reduce[ , -c(3,4)], 
                           key = Word, value = Percent, fill = 0)
rownames(bag_words_spread) <- bag_words_spread[ , 1]
bag_words_dist <- get_dist(bag_words_spread[ , -1])

fviz_dist(bag_words_dist)  

bag_words_mds <- cmdscale(bag_words_dist, #distances
                          k = 2, #number of dimensions
                          eig = T #calculate the eigenvalues
)
barplot(bag_words_mds$eig)

{plot(bag_words_mds$points, #plot the MDS dimension points
      type = "n", #blank canvas plot
      main = "MDS Bag Words")
  
  text(bag_words_mds$points, #plot the dimensions
       labels = bag_words_spread[ , 1], #label them with the names
       cex = .6) #text sizing
}

#run the MDS
bag_words_mds_3d = cmdscale(bag_words_dist, k = 3, eig = TRUE)

#plot 3d
library(rgl)
{
  plot3d(bag_words_mds_3d$points, type = "n")
  text3d(bag_words_mds_3d$points, texts = bag_words_spread[ , 1], cex = .6)
}


# k means with removal ----------------------------------------------------

category_combos <- list()
bag_words_reduce <- subset(bag_words, Percent >= 10)

for (i in 1:5){
  
  bag_words_spread <- spread(bag_words_reduce[ , -c(3,4)], key = Word, value = Percent, fill = 0)
  bag_words_cluster <- kmeans(bag_words_spread[ , -1], 
                              centers = 20,
                              nstart = 25)
  bag_words_category <- as.data.frame(bag_words_cluster$cluster)
  bag_words_category$feature <- bag_words_spread$Feature
  colnames(bag_words_category)[1] <- "category"
  cat_temp <- as.data.frame(table(bag_words_category$category))
  keep <- cat_temp$Var1[cat_temp$Freq >= 10 & cat_temp$Freq<500]
  category_combos[[i]] <- subset(bag_words_category, category %in% keep)
  
  #remove words and start over
  bag_words_reduce <- bag_words_reduce[!(bag_words_reduce$Feature %in% unique(category_combos[[i]]$feature)) , ]
}