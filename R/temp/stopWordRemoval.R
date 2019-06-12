# Stopwords and Other Exclusions ------------------------------------------
library('stopwords')
library('stringi')

## Remove stopwords from lemmas
X <- read.csv("./output_data/spellchecked.features.csv", stringsAsFactors = F)
X.nostop <- X
X.nostop$feature <- stri_replace_all_regex(X$feature,pattern = paste(stopwords(language = "en",
                                         source = "snowball"),collapse = "\\b|\\b"),
                                         replacement = "",F, list(case_insensitive = TRUE))
# Remove trailing and double spaces
X.nostop$feature <- str_replace(gsub("\\s+", " ", str_trim(X.nostop$feature)), "B", "b")
write.csv(x = X.nostop,file = './output_data/spellchecked.nostop.features.csv',fileEncoding = 'utf8',row.names = F)


## Idem for multiwords features
X <- read.csv("./output_data/lemmatized.features.csv", stringsAsFactors = F)
X.nostop <- X
X.nostop <- subset(X.nostop,!(feature %in% stopwords(language = "en", source = "snowball")))
X.nostop$feature <- str_replace(gsub("\\s+", " ", str_trim(X.nostop$feature)), "B", "b")
write.csv(x = X.nostop,file = './output_data/lemmatized.nostop.features.csv',fileEncoding = 'utf8',row.names = F)

