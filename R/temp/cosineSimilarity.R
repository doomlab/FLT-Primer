library('lsa')

X <- read.csv("./output_data/lemmatized.nostop.bag.csv", stringsAsFactors = F)
unique_cues = unique(X$cue)


# Comparison to MEN -------------------------------------------------------
men_data <- read.table("../data/MEN_dataset_natural_form_full.txt",quote="\"", comment.char="", stringsAsFactors=FALSE)

bag_words_spread <- spread(X, key = cue, value = frequency, fill = 0)
bag_words_cosine <- cosine(as.matrix(bag_words_spread[ , -1]))

bag_words_cosine <- as_tibble(bag_words_cosine)
bag_words_cosine$cue <- colnames(bag_words_cosine)
bag_words_cosine <- gather(bag_words_cosine, key =feature,value = cosine,abstract:zebra)

# Use a join instead
bag_words_cosine$key <- paste(bag_words_cosine$cue, bag_words_cosine$feature, sep = " ")
men_data$key <- paste(men_data$V1, men_data$V2)

colnames(men_data) <- c("cue", "target", "rating", "key")
men_merge = merge(men_data, bag_words_cosine, by = "key")

# Consider spreading activation
cor(men_merge$rating,men_merge$cosine)

# .72 for lemmatized