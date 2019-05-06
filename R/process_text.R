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

### Loop over the data.frame
for (i in 1:nrow(lemmas)){
  ### Replace with lemmas
  stem_suggest <- hunspell_stem(unlist(hunspell_parse(lemmas$answer[i])))
  lemmas$answer[i] <- paste0(unlist(lapply(stem_suggest, tail, n = 1L)), collapse = " ")
}

## Install the koRpus package
#install.packages("koRpus")

library(koRpus)



tagged.results <- treetag(c("wings", "run", "ran", "cheese"), 
                          treetagger="manual", format="obj",
                          TT.tknz=FALSE, lang="en",
                          TT.options=list(path="~/TreeTagger", preset="en"))
tagged.results@TT.res
