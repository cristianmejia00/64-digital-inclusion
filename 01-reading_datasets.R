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
documents <- apply(dataset[c("TI", "AB", "DE", "ID")], 1, paste, collapse = " ") # Add "AB"
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
DXK <- lapply(keywords_stems_df$stems, function(x) {
  myPattern = paste(" ", x, " ", sep = "")
  return(str_count(documents_stems, pattern = myPattern))
})
DXK <- DXK |> unlist() |> matrix(nrow = length(documents_stems), 
                                   ncol = length(keywords_stems_df$stems),
                                   dimnames = list(c(),keywords_stems_df$raw))

# As data frame
DXK <- as.data.frame.matrix(DXK)

# Create a backup
if (!exists("DXK_backup")) { DXK_backup <- DXK }

# Optional. Load from backup
#DXK <- DXK_backup
################################################ Cleaning the matrix
# Remove stopwords from the matrix
DXK <- DXK[,! colnames(DXK) %in% stopwords("en")]
DXK <- DXK[,! colnames(DXK) %in% stopwords("SMART")]


# Compute tfidf and remove keywords based on tfidf stats
keywords_frequency <- colSums(DXK) |> sort(decreasing = TRUE)
keywords_frequency <- data.frame("keywords" = names(keywords_frequency),
                   "freq" = unname(keywords_frequency))

keywords_doc_frequency <- DXK > 0
keywords_doc_frequency <- colSums(keywords_doc_frequency) |> sort(decreasing = TRUE)
keywords_doc_frequency <- data.frame("keywords" = names(keywords_doc_frequency),
                                     "docs" = unname(keywords_doc_frequency))


# Remove keywords with more than 2 docs matching. Because they appear too infrequent
DXK <- DXK[,colnames(DXK) %in% keywords_doc_frequency$keywords[keywords_doc_frequency$docs > 2]]

# Remove keywords appearing in more than 80% of documents. Because they appear too frequently
DXK <- DXK[,! colnames(DXK) %in% keywords_doc_frequency$keywords[keywords_doc_frequency$docs > (nrow(dataset) * 0.8)]]

# Remove keywords from custom stopwords known to be unuseful
custom_stopwords_academic <- c("al.", "research", "analysis", "paper", "study", "nots")
DXK <- DXK[,! colnames(DXK) %in% custom_stopwords_academic]
                                
# Compute tfidf                                
keywords_tfidf <- merge(keywords_frequency, keywords_doc_frequency)
keywords_tfidf <- keywords_tfidf[keywords_tfidf$keywords %in% colnames(DXK),]
keywords_tfidf$tf <- keywords_tfidf$freq / sum(keywords_tfidf$freq)
keywords_tfidf$idf <- log(nrow(dataset) / keywords_tfidf$docs)
keywords_tfidf$tfidf <- keywords_tfidf$tf * keywords_tfidf$idf
keywords_tfidf <- keywords_tfidf[order(keywords_tfidf$tfidf, decreasing = TRUE),]




################################################# document abstract with only valid stems
# Create the WOS formatted keyword string from a row from the document x keywords matrix

# Util function to get the strings
create_keyword_string <- function(a_row) {
  tmp <- a_row[a_row > 0]
  if (length(tmp) > 0) {
    tmp_str <- lapply(c(1:length(tmp)), function(x){
      return(rep(names(tmp[x]), tmp[x]))
    }) |> unlist() |> paste(collapse = "; ")    
  } else {
    tmp_str <- ""
  }
  return(tmp_str)
}

DXK <- as.matrix(DXK)

# Get the column. This replaces the WOS keywords
keywords_column <- lapply(c(1:nrow(DXK)), function(y) {
  this_row <- DXK[y,]
  this_row_kwds <- create_keyword_string(this_row)
  return(this_row_kwds)
}) |> unlist()

# change the name of the WOS keywords not to lose the data
dataset <- dataset[,c(1:39)]
setnames(dataset, "ID", "ID_WOS")

# Append the column as if were the WOS keywords column
# So we can use this string to create networks in VosViewer
dataset$ID <- keywords_column

# Write the file to be used in VosViewer
write.csv(dataset[,setdiff(colnames(dataset), c("X_N","X_E","X_D","X_C"))], file = "dataset_vos.csv", row.names = FALSE)

################################################# create reports
### Report of Document x Keyword
dataset$NN <- c(1:nrow(dataset))
dataset_valid_columns <- dataset[,c("NN","UT","TI","AB","PY","X_E","Z9","X_C")]
setnames(dataset_valid_columns, c("NN", "X_E", "Z9", "X_C"), c("N", "Degree", "Citations", "Fukan_Cluster"))

dataset_report <- cbind(dataset_valid_columns, DXK)

# Write the report for Ranjit
write.csv(dataset_report, file = "documents_x_keywords.csv", row.names = FALSE)
write.csv(keywords_tfidf, file = "keywords_tfidf.csv", row.names = FALSE)

################################################## Difference AB and no AB keywords
# Find the difference between the keywords considering the abstract and not.

k_tfidf_wo_ab # Use readr to load the .csv file `keywords_tfidf.csv` for the results without abstract
k_tfidf_ab <- keywords_tfidf 

k_tfidf_difference <- k_tfidf_ab[!k_tfidf_ab$keywords %in% k_tfidf_wo_ab$keywords,]
write.csv(k_tfidf_difference, file = "keywords_difference.csv", row.names = FALSE)
