
library(tidytext)
library(koRpus)
library(koRpus.lang.en)
library(tokenizers)

X <- read.csv("./output_data/spellchecked.features.csv", stringsAsFactors = F)


# Extract a list of words
tokens = unnest_tokens(tbl = X, output = word, input = feature)
wordlist = unique(tokens$word)

## Note: the NULL option is to control for the <unknown> that appears
## to occur with the last word in each text

## Install TreeTagger
#https://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/
#tagged.features <- taggedText(suppressWarnings(
# treetag(c(wordlist, "NULL"), treetagger="manual", format="obj",TT.tknz=FALSE, lang="en",
#        TT.options=list(path="./scripts/treetagger", preset="en",params='english-bnc.par'),encoding='UTF-8',doc_id = NA)))

# select relevant columns
#tagged.features = tagged.features %>% select(token,lemma,tag,wclass,sntc)


# Taken from Erin's code
unique_concepts = unique(X$cue)

multi_words <- data.frame(cue=character(),
                          feature=character(),
                          frequency=numeric(),
                          stringsAsFactors=FALSE)


## Loop over each word
for (i in 1:length(unique_concepts)){

  ## Create parts of speech for clustering together
  temp_tag <- suppressWarnings(
    suppressMessages(
      treetag(c(X$feature[X$cue  == unique_concepts[i]], "NULL"),
              treetagger="manual", format="obj",
              TT.tknz=FALSE, lang="en",doc_id = unique_concepts[i],
              TT.options=list(path="./scripts/treetagger", preset="en"))))

  temp_tag <- taggedText(temp_tag)

  # Remove certain word classes
  remove.wclass = c('comma','determiner','article','preposition','predeterminer','particle',
                                        'to','punctuation','fullstop','conjunction','pronoun')

  temp_tag <- temp_tag %>% dplyr::filter(!lemma == '<unknown>') %>% dplyr::filter(!wclass %in% remove.wclass)


  ## Create a temporary tibble
  temp_tag_tibble <- as_tibble(temp_tag)

  ## Combine features and POS
  temp_tag_tibble <- mutate(temp_tag_tibble,
                            two_words = paste(token, lead(token), sep = "_"))

  temp_tag_tibble <- mutate(temp_tag_tibble,
                            three_words = paste(token, lead(token), lead(token, n = 2L), sep = "_"))

  temp_tag_tibble <- mutate(temp_tag_tibble,
                            two_words_pos = paste(wclass, lead(wclass), sep = "_"))

  temp_tag_tibble <- mutate(temp_tag_tibble,
                            three_words_pos = paste(wclass, lead(wclass), lead(wclass, n = 2L), sep = "_"))

  ## Patterns
  adverb_adj <- grep("\\badverb_adj", temp_tag_tibble$two_words_pos)
  verb_nouns <- grep("\\bverb_noun", temp_tag_tibble$two_words_pos)
  verb_adj_nouns <- grep("\\bverb_adjective_noun", temp_tag_tibble$three_words_pos)

  ## Use combined and left over features
  features_for_table <- c(temp_tag_tibble$two_words[adverb_adj],
                          temp_tag_tibble$two_words[verb_nouns],
                          temp_tag_tibble$three_words[verb_adj_nouns],
                          temp_tag_tibble$token[-c(verb_nouns, verb_nouns+1,
                                                   verb_adj_nouns, verb_adj_nouns+1, verb_adj_nouns+2)])

  ## Create a table of frequencies
  word_table <- as.data.frame(table(features_for_table))

  ## Clean up the table (fix this with dplyr)
  word_table$cue <- unique_concepts[i]
  colnames(word_table) = c("feature", "frequency", "cue")
  multi_words <- rbind(multi_words, word_table[ , c(3, 1, 2)])

}

# Write processed file
write.csv(x = multi_words,file = './output_data/lemmatized.features.csv',fileEncoding = 'utf8',row.names = F)


