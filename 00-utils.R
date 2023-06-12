# 20230509

# Copy of read from Fukan
open_from_fukan_2 <- function(a_path) {
  # Function to read data from Fukan
  
  # Read the dataset
  dataset <- read.table(a_path, sep = '\t', fill = TRUE, stringsAsFactors = FALSE, header = FALSE, check.names = FALSE, quote = "")
  colnames(dataset) <- dataset[1,]
  dataset <- dataset[-1,]
  
  # Adjust header names
  setnames(dataset, c('_N', '_C', '_D', '_E'), c('X_N', 'X_C', 'X_D', 'X_E'), skip_absent = TRUE)
  
  # Keep potentially usable columns
  usable_columns <- c('X_N', 'X_C', 'X_D', 'X_E',
                      "PT", "AU", "TI", "SO", "LA", "DT", "DE", "ID", "AB", "C1", "OI", "AF", 
                      "RP", "FU", "FX", "CR", "NR", "TC", "Z9", "U1", "U2", "PU", "SN", "J9",  
                      "JI", "PY", "VL", "IS", "BP", "EP", "DI", "PG", "WC", "SC","UT")
  available_usable_columns <- intersect(usable_columns, colnames(dataset))
  dataset <- dataset[,available_usable_columns]
  
  # Force as data.frame type
  dataset <- as.data.frame(dataset)
  
  # Format classes
  numeric_columns <- intersect(c('X_N', 'X_C', 'X_D', 'X_E', "NR", "TC", "Z9", "U1", "U2", "PY"), available_usable_columns)
  for (i in numeric_columns) {
    dataset[,i] <- as.numeric(dataset[,i])
    print(i)
    print(class(dataset[,i]))
  }
  
  # All format to lower case
  text_columns <- intersect(c("AU", "TI", "SO", "LA", "DT", "DE", "ID", "AB", "C1", "AF", "RP", "PU", "FU", "FX", "J9", "JI"), available_usable_columns)
  for (i in text_columns) {
    dataset[,i] <- tolower(dataset[,i])
  }
  
  # Fill-in missing UT fields. We fill them with their row number (Can be anything, as is non repeated)
  if ("UT" %in% available_usable_columns) {
    blank_UT <- which(dataset$UT == "")
    dataset$UT[blank_UT] <- blank_UT
  }
  
  # Remove rows not numbered by Fukan System
  if ("X_N" %in% available_usable_columns) {
    dataset <- dataset[!duplicated(dataset$X_N),]
    dataset <- dataset[!is.na(dataset$X_N),]
  }
  
  # return dataset
  return(dataset)
}



##################################################
# Functions that facilitate identifying meaningless keywords by TFIDF threshold
stemmedCorpus <- function(myCorpus) {
  myCorpus <- tm_map(myCorpus, content_transformer(tolower)) |>
              tm_map(removePunctuation) |>
              tm_map(removeNumbers) |>
              tm_map(stemDocument, language = "english") |>
              tm_map(stripWhitespace) |>
              tm_map(stripWhitespace)
  return(myCorpus)
}

# Transform a tidy corpus to a vector of normal, but cleaned text.
# Inputs: The result from tidyCorpus()
# Output: A vector of characters. The text to be used in the topic model
corpusToText <- function(a_tidyCorpus) {
  text <- unlist(sapply(1:length(a_tidyCorpus), function(x){return(a_tidyCorpus[[x]]$content)}))
  return(text)
}

# Function to remove the copyright from abstracts.
# Input: A char string or vector. Ususally the abstracts from WOS
# Output: The imput without the copyritgh statements.
# Dependencies: None.
remove_copyright_statements <- function(a_text) {
  return(gsub(" [A-z0-9 ]*?\\(C\\).*$", "", a_text))
}



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