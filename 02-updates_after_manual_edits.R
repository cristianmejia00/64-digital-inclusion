# 20230612

# This file corresponds to the second and subsequent rounds of cleaning. 
# Ranjit edited went trough the previous reports and identified:
# - Keywords that must be deleted
# - Keywords that must be unified

# Here, we apply these changes. And create the report as Ranjit needs it. 

#---------------------------

# INPUTS:
# The environ of the first step
load("env20230510_withoutabstract.RData")

# The file edited by Ranjit. 
# He provided an Excel file from which we extract the sheet we need as a `.csv`

file.choose()
dataset_edited <- read.csv("C:\\Users\\crist\\Desktop\\documents_x_keywords_20230623_ranjit.csv")

#---------------------------
# `keywords_groups` refers to the groups of keywords Ranjit intends to merge.
keywords_groups <- dataset_edited$Merge |> unique()

# Clean
keywords_groups <- keywords_groups[keywords_groups != '']

# Custom corrections
# ! For this test run we are merging only the ICT one so we simplify it to contain only ICT
keywords_groups <- keywords_groups[keywords_groups == 'ICT']


# Find the raw keywords that correspond to each keyword group
keywords_grouped <- lapply(keywords_groups, function(x) {
  raws <- data.frame(dataset_edited$original.keywords[dataset_edited$Merge == x]) |> unlist()
  stems <- keywords_stems_df$stems[match(raws, keywords_stems_df$raw)]
  main_stem <- keywords_stems_df$stems[which(keywords_stems_df$raw == raws[1])]
  tmp = data.frame("keywords" = raws,
                   "stems" = stems,
                   "main_stem" = main_stem,
                   "group" = x)
  return(tmp)
}) %>% rbindlist()

# Transform the stems to the corresponding main_stem
documents_stems_edited <- documents_stems
for (i in c(1:length(keywords_grouped$keywords))) {
  my_pattern <- paste(" ", keywords_grouped$stem[i], " ", sep = "")
  my_correction <- paste(" ", keywords_grouped$main_stem[i], " ", sep = "")
  documents_stems_edited <- gsub(my_pattern, my_correction, documents_stems_edited)
}

# Find the list of keywords Ranjit marked as exclude
ranjit_stopwords <- dataset_edited$original.keywords[dataset_edited$Exclude == "y"]
