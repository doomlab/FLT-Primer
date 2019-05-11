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


# Bag of Words ------------------------------------------------------------

bag_words <- data.frame(Word=character(),
                        Feature=character(), 
                        Frequency=numeric(), 
                        stringsAsFactors=FALSE) 

unique_concepts <- unique(no_stop$word)

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

# Multi-Word Strings -----------------------------------------------------

multi_words <- data.frame(Word=character(),
                        Feature=character(), 
                        Frequency=numeric(), 
                        stringsAsFactors=FALSE) 
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

## Remove stop words
multi_words <- subset(multi_words, 
                      !(Feature %in% stopwords(language = "en", source = "snowball")))


