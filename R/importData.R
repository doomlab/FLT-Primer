# Importing raw data ------------------------------------------------------
source("../R/dependencies.R")

# Importing the raw feature lists
X <- read.csv("../raw_data/tidy_words.csv", stringsAsFactors = F)

## Lower case to normalize
X$feature_response <- tolower(X$feature_response)
