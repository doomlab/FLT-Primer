# Perform spell checking on the raw feature listings consisting of a word column and an answer column

library(here)
library(hunspell)

# Importing the raw feature lists
X <- read.csv("./raw_data/tidy_words.csv", stringsAsFactors = F)

# Extract a list of words
tokens = unnest_tokens(tbl = X, output = token, input = answer)
wordlist = unique(tokens$token)

# Spell check the words
spelling.errors <- hunspell(wordlist)
spelling.errors <- unique(unlist(spelling.errors))
spelling.sugg <- hunspell_suggest(spelling.errors, dict = dictionary("en_US"))

# Pick the first suggestion
spelling.sugg <- unlist(lapply(spelling.sugg, function(x) x[1]))
spelling.dict <- as.data.frame(cbind(spelling.errors,spelling.sugg))
spelling.pattern <- paste0("\\b", spelling.errors, "\\b")

# Parse features
tokens <- unnest_tokens(tbl = X, output = token, input = answer, token = stringr::str_split, pattern = "  |\\, |\\.|\\,|\\;")
tokens$token <- trimws(tokens$token, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")

# Remove empty features
tokens <- tokens[!tokens$token =='',]
tokens$corrected = stri_replace_all_regex(tokens$token, spelling.pattern,spelling.sugg, vectorize_all = FALSE)

# Rename columns
tokens = tokens %>% rename(cue=word,feature=corrected) %>% select(cue,feature)

# Write processed file
write.csv(x = tokens,file = './output_data/spellchecked.features.csv',fileEncoding = 'utf8',row.names = F)
