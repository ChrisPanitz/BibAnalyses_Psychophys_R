### header ###
minOccur_multi <- 5 # minimum of occurrences of multi-word combinations to be classified as key term
minOccur_export <- 5 # minimum of occurrences of key terms to be exported for dictionary
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




# check for common 2-/3-/4-word combinations
# termExtraction only shows top results, however, tableTag does not include the option to spare number
# tableTag is used to extract multi-word key terms for double-checking but termExtraction can be used to manually screen for important numeric word combinations
termExtraction(df, Field = "TI", ngrams = 4, remove.numbers = FALSE, verbose = TRUE)
tabTags4 <- tableTag(df, Tag = "TI", ngrams = 4)
termExtraction(df, Field = "TI", ngrams = 3, remove.numbers = FALSE, verbose = TRUE)
tabTags3 <- tableTag(df, Tag = "TI", ngrams = 3)
termExtraction(df, Field = "TI", ngrams = 2, remove.numbers = FALSE, verbose = TRUE)
tabTags2 <- tableTag(df, Tag = "TI", ngrams = 2)
tableTag(df, Tag = "TI", ngrams = 1)

df <- termExtraction(df, Field = "TI", ngrams = 1, remove.numbers = FALSE, verbose = TRUE)

# make vector with all 4-to-2 word combinations with at least [minOccur_multi] occurrences
terms2keep_TI <- c(dimnames(tabTags4[tabTags4 >= minOccur_multi])[["Tab"]],
                   dimnames(tabTags3[tabTags3 >= minOccur_multi])[["Tab"]],
                   dimnames(tabTags2[tabTags2 >= minOccur_multi])[["Tab"]])

write.table(terms2keep_TI, paste0(parentFolder, "/terms/compoundTerms_TI_selected.txt"),
            row.names = FALSE, col.names = FALSE)

# look in the TI_TM field for multi-word terms and replace ";" by "-"
replace_TI <- gsub(" ",";",terms2keep_TI)
replaceWith_TI <- gsub(" ","-",terms2keep_TI)

for (i in 1:length(terms2keep_TI)){
  df$TI_TM <- gsub(replace_TI[i],replaceWith_TI[i],df$TI_TM)
}

# write all key terms into text file (to create dictionary)
tabTags <- tableTag(df, Tag = "TI_TM")
write.table(tabTags[tabTags >= minOccur_export],
            "terms/keyTerms_TI_raw.txt", sep = ";", row.names = FALSE, col.names = c("keyterm","count"))