# Lemmatization and Multi-Word Sequences ----------------------------------
source("./R/dependencies.R")

# Open the spell checked data
X <- read.csv("./output_data/spellchecked.features.csv", stringsAsFactors = F)

# Extract the list of updated tokens
tokens <- unnest_tokens(tbl = X, output = word, input = feature)
cuelist <- unique(tokens$cue)

# Create a dataframe for lemmas
tokens.tagged <- data.frame(doc_id=character(),
                          token=character(),
                          wclass=character(),
                          lemma=character(),
                          stringsAsFactors=FALSE)

# Loop over cues and create lemmas + POS tags 
for (i in 1:length(cuelist)){
  
  temp.tag <- suppressWarnings(
    suppressMessages(
      treetag(c(X$feature[X$cue == cuelist[i]], "NULL"),
              treetagger="manual", format="obj",
              TT.tknz=FALSE, lang="en", doc_id = cuelist[i],
              # These parameters are based on your computer
              TT.options=list(path="~/TreeTagger", preset="en"))))
  
  temp.tag <- temp.tag@TT.res %>% 
    mutate_if(is.factor, as.character)
  
  tokens.tagged <- tokens.tagged %>% 
    bind_rows(temp.tag %>% 
                select(doc_id, token, wclass, lemma))
  }

tokens.tagged <- tokens.tagged %>% 
  rename(cue = doc_id, feature = token, pos = wclass)

# Clean up unknown lookups
tokens.tagged$lemma[tokens.tagged$lemma == "<unknown>"] <- tokens.tagged$feature[tokens.tagged$lemma == "<unknown>"]
tokens.tagged$lemma[tokens.tagged$lemma == "@card@"] <- tokens.tagged$feature[tokens.tagged$lemma == "@card@"]
tokens.tagged$lemma <- tolower(tokens.tagged$lemma)

# Write processed file
write.csv(x = tokens.tagged, file = "./output_data/lemmatized.features.csv", 
          fileEncoding = "utf8", row.names = F)

