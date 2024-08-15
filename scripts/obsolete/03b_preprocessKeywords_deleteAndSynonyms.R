### header ###
minOccur <- 10 # minimum of key word occurrences to be included
### end header ###

# load packages
library(here)
library(bibliometrix)
library(dplyr)

# parent folder
parentFolder <- here()

# load  bibliometric data frame
filename <- paste0(parentFolder, "/rawData/allPapers.rds")
df <- readRDS(filename)

# extract keywords from article titles
df <- termExtraction(df, Field = "TI", ngrams = 1, remove.numbers = FALSE, verbose = TRUE)

# load multi-word key terms to keep
terms2keep_TI <- read.table("terms/compoundTerms_TI_selected.txt", sep = ";")[,1]

# custom way to treat multi-word terms as one term, words joint by hyphen
replace2keep_TI <- gsub(" ",";",terms2keep_TI)
replaceWith_TI <- gsub(" ","-",terms2keep_TI)
for (i in 1:length(terms2keep_TI)){
  df$TI_TM <- gsub(replace2keep_TI[i],replaceWith_TI[i],df$TI_TM)
}

tabTags <- tableTag(df, Tag = "TI_TM")
write.table(tabTags[tabTags >= minOccur],
            "terms/keyTerms_TI_raw.txt", sep = ";", row.names = FALSE, col.names = c("keyterm","count"))

tabTags[tabTags >= minOccur]
test <- tabTags[tabTags >= minOccur]
tabTags[1:120]
