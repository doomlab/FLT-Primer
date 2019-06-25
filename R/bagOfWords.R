# Bag of Words ------------------------------------------------------------
source("./R/dependencies.R")

# Open the no stop words data
X <- read.csv("./output_data/nostop.lemmas.csv", stringsAsFactors = F)

# Create cue-lemma frequency 
bag.words <- X %>% 
  group_by(cue) %>% 
  count(lemma) 

# Write processed file
write.csv(x = bag.words, file = "./output_data/bag.nostop.lemmas.csv",
          fileEncoding = "utf8", row.names = F)
