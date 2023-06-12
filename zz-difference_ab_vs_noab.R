# ################################################## Difference AB and no AB keywords
# # Find the difference between the keywords considering the abstract and not.
# 
# k_tfidf_wo_ab # Use readr to load the .csv file `keywords_tfidf.csv` for the results without abstract
# k_tfidf_ab <- keywords_tfidf 
# 
# k_tfidf_difference <- k_tfidf_ab[!k_tfidf_ab$keywords %in% k_tfidf_wo_ab$keywords,]
# write.csv(k_tfidf_difference, file = "keywords_difference.csv", row.names = FALSE)