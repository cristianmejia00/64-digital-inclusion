
# If we applied ranjit edits, then use those edits from now on
# and save the original as a different object
if (exists("documents_stems_edited")) {
  documents_stems_original <- documents_stems
  documents_stems <- documents_stems_edited
}

# Create the Document x Term matrix
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
if (exists("ranjit_stopwords")) {
  DXK <- DXK[,! colnames(DXK) %in% ranjit_stopwords]
}

# Compute tfidf and remove keywords based on tfidf stats
keywords_frequency <- colSums(DXK) |> sort(decreasing = TRUE)
keywords_frequency <- data.frame("keywords" = names(keywords_frequency),
                                 "freq" = unname(keywords_frequency))

keywords_doc_frequency <- DXK > 0
keywords_doc_frequency <- colSums(keywords_doc_frequency) |> sort(decreasing = TRUE)
keywords_doc_frequency <- data.frame("keywords" = names(keywords_doc_frequency),
                                     "docs" = unname(keywords_doc_frequency))


# Remove keywords with less than 2 docs matching. Because they appear too infrequent
DXK <- DXK[,colnames(DXK) %in% keywords_doc_frequency$keywords[keywords_doc_frequency$docs > 2]]

# Remove keywords appearing in more than 80% of documents. Because they appear too frequently
# We only do it the first time. 
if (!exists("documents_stems_edited")) {
  DXK <- DXK[,! colnames(DXK) %in% keywords_doc_frequency$keywords[keywords_doc_frequency$docs > (nrow(dataset) * 0.8)]]
}

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


# Find if the keyword belong to a group ("merged" in Ranjit's report)
keywords_tfidf$merged <- ""
keywords_tfidf$merged[keywords_tfidf$keywords %in% keywords_grouped$keywords] <- keywords_grouped$group[match(keywords_tfidf$keywords[keywords_tfidf$keywords %in% keywords_grouped$keywords], keywords_grouped$keywords)]


# Cumpute the summary of documents per cluster per keyword
paper_per_cluster <- table(dataset$X_C)

cluster_kwd_summary <- lapply(keywords_tfidf$keywords, function(x) {
  this_stem <- keywords_stems_df$stem[keywords_stems_df$raw == x]
  papers_per_kwd_per_cluster <- table(dataset$X_C[grepl(this_stem, documents_stems)])
  prop <- (papers_per_kwd_per_cluster / paper_per_cluster[as.numeric(names(papers_per_kwd_per_cluster))]) * 100 |> round(1)
  
  tmp1 <- as.data.frame.list(papers_per_kwd_per_cluster, col.names = paste("papers_cl_", names(papers_per_kwd_per_cluster), sep = ""))
  tmp2 <- as.data.frame.list(prop, col.names = paste("percent_cl_", names(prop), sep = ""))
  tmp <- cbind(tmp1, tmp2)
  
  return(tmp)
}) |> rbindlist(fill = TRUE)
cluster_kwd_summary[is.na(cluster_kwd_summary)] <- 0

# Append the cluster count per keyword
keywords_tfidf <- cbind(keywords_tfidf, cluster_kwd_summary)


################################################# document with only valid stems
# Create the WOS formatted keyword string from a row from the document x keywords matrix

DXK <- as.matrix(DXK)

# Get the column. This replaces the WOS keywords
keywords_column <- lapply(c(1:nrow(DXK)), function(y) {
  this_row <- DXK[y,]
  this_row_kwds <- create_keyword_string(this_row)
  return(this_row_kwds)
}) |> unlist()


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
