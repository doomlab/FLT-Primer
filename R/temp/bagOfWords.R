
# Bag of Words ------------------------------------------------------------

bag_words <- data.frame(cue=character(),
                        feature=character(),
                        frequency=numeric(),
                        stringsAsFactors=FALSE)

X <- read.csv("./output_data/spellchecked.nostop.features.csv", stringsAsFactors = F)
unique_cues = unique(X$cue)


# This could be done with tidyverse (check spellCheck for example)
## Loop over each word
for (i in 1:length(unique_cues)){

  ## Create a table of frequencies
  word_table <- as.data.frame(table(
    unlist(
      ## Tokenize the words
      tokenize_words(
        ## Put all answers together in one character string
        paste0(X$feature[X$cue == unique_cues[i]], collapse = " ")))))

  ## Clean up the table
  word_table$cue <- unique_cues[i]
  colnames(word_table) = c("feature", "frequency", "cue")
  bag_words <- rbind(bag_words, word_table[ , c(3, 1, 2)])

}

write.csv(x = bag_words,file = './output_data/spellchecked.nostop.bag.csv',fileEncoding = 'utf8',row.names = F)

# Do the same for lemmatized, put in function
#bag = unnest_tokens(tbl = X, output = feature, input = feature)


# Bag of Words ------------------------------------------------------------
bag_words <- data.frame(cue=character(),
                        feature=character(),
                        frequency=numeric(),
                        stringsAsFactors=FALSE)

X <- read.csv("./output_data/lemmatized.nostop.features.csv", stringsAsFactors = F)
unique_cues = unique(X$cue)


# This could be done with tidyverse (check spellCheck for example)
## Loop over each word
for (i in 1:length(unique_cues)){

  ## Create a table of frequencies
  word_table <- as.data.frame(table(
    unlist(
      ## Tokenize the words
      tokenize_words(
        ## Put all answers together in one character string
        paste0(X$feature[X$cue == unique_cues[i]], collapse = " ")))))

  ## Clean up the table
  word_table$cue <- unique_cues[i]
  colnames(word_table) = c("feature", "frequency", "cue")
  bag_words <- rbind(bag_words, word_table[ , c(3, 1, 2)])

}

write.csv(x = bag_words,file = './output_data/lemmatized.nostop.bag.csv',fileEncoding = 'utf8',row.names = F)
