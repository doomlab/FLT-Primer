# Multi-word sequences ------------------------------------------------------
source("./R/dependencies.R")

# Open the no stop words data
X <- read.csv("./output_data/nostop.lemmas.csv", stringsAsFactors = F)

# Combine lemmas and POS
X <- X %>% 
  mutate(two.words = paste(lemma, lead(lemma), sep = " "), 
         three.words = paste(lemma, lead(lemma), 
                             lead(lemma, n = 2L), sep = " "), 
         two.words.pos = paste(pos, lead(pos), sep = "."),
         three.words.pos = paste(pos, lead(pos), 
                                 lead(pos, n = 2L), sep = "."))

# Patterns
adverb.adj <- grep("\\badverb.adj", X$two.words.pos)
verb.nouns <- grep("\\bverb.noun", X$two.words.pos)
verb.adj.nouns <- grep("\\bverb.adjective.noun", X$three.words.pos)

# Use combined and left over lemmas
X$combined.lemmas <- NA
X$combined.lemmas[c(adverb.adj, verb.nouns)] <- X$two.words[c(adverb.adj,verb.nouns)]
X$combined.lemmas[verb.adj.nouns] <- X$three.words[verb.adj.nouns]
X$combined.lemmas[-c(verb.nouns, verb.nouns+1, verb.adj.nouns, 
                     verb.adj.nouns+1, verb.adj.nouns+2)] <- X$lemma[-c(verb.nouns, verb.nouns+1,
                                                                        verb.adj.nouns, verb.adj.nouns+1, 
                                                                        verb.adj.nouns+2)]

#Create cue-lemma frequency
multi.words <- X %>% 
  filter(!is.na(combined.lemmas)) %>% 
  group_by(cue) %>% 
  count(combined.lemmas) 

# Write processed file
write.csv(x = multi.words, file = "./output_data/multi.nostop.lemmas.csv",
          fileEncoding = "utf8", row.names = F)
