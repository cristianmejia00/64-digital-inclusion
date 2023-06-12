# 20230509
# Cristian Mejia -- cristianmejia00 [at] gmail.com

# Reading the facet file from Fukan System analysis
# d46990: Q266 - TS="digital inclusion"
# https://academic-landscape.com/analysis/46865/0#c0

## Read facets
load("dataset.rdata")
#dataset <- open_from_fukan_2(file.choose())

## Read util functions
source("00-utils.R")

## Read keywords
mission_keyword_all <- read_delim("C:/Users/crist/Desktop/a46865/mission.keyword.all.tsv", 
                                  delim = "\t", escape_double = FALSE, 
                                  trim_ws = TRUE)

## Get vector of valid keywords
# Valid keywords are those appearing in 2 documents or more
valid_keywords <- mission_keyword_all$TERM[mission_keyword_all$DC >= 2]


## Custom stopwords
custom_stopwords_query <- c("digital","inclusion","digital inclusion")

## Update the valid keywords by removing stopwords
valid_keywords <- valid_keywords[! valid_keywords %in% custom_stopwords_query]


################################################# keyword vector
keywords_corpus <- SimpleCorpus(VectorSource(enc2utf8(valid_keywords)))
keywords_stems <- stemmedCorpus(keywords_corpus) |> corpusToText()
keywords_stems_df <- data.frame("stems" = keywords_stems,
                                "raw" = valid_keywords)
keywords_stems_df <- keywords_stems_df[!duplicated(keywords_stems_df$stems),]

################################################# document stems
documents <- apply(dataset[c("TI", "DE", "ID")], 1, paste, collapse = " ") # Add "AB"
documents_corpus <- SimpleCorpus(VectorSource(enc2utf8(documents)))
documents_stems <- stemmedCorpus(documents_corpus) |> corpusToText()
documents_stems[2]

################################################# document only valid stems
# Hacky warning. Append an space at the end of the documents to facilitate matching the patterns.
# The patterns are the stems + white space. This pattern works this way to avoid matching subpatterns within
# larger keywords. For example if we search for "soci" we do not want to count the soci in "societi". 
# however, the pattern "soci " is not in "societi".
# Therefore, the space at the end of documents is needed in case that the pattern we look for is, by chance, the very last word in the paragraph.
documents_stems <- paste(" ", documents_stems, " ", sep = "")


