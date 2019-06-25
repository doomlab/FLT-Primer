# Stopwords and Other Exclusions ------------------------------------------
source("./R/dependencies.R")

# Open the lemmatized data
X <- read.csv("./output_data/lemmatized.features.csv", stringsAsFactors = F)

# Remove punctuation and stopwords from lemmas
X$lemma <- gsub("\\-", " ", X$lemma)
X$lemma <- gsub("^$|\002", NA, trimws(X$lemma))

X.nostop <- X %>% 
  filter(!grepl("[[:punct:]]", lemma)) %>% 
  filter(!lemma %in% stopwords(language = "en", source = "snowball")) %>% 
  filter(!is.na(lemma)) 

# Write processed file
write.csv(x = X.nostop, file = "./output_data/nostop.lemmas.csv", 
          fileEncoding = "utf8", row.names = F)
