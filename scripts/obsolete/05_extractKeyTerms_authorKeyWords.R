### header ###
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

# in some instances, hyphens were replaced by there Unicode (e.g., EVENT&#8208; RELATED instead of EVENT-RELATED)
# fixing this:
df$DE <- gsub("&#8208; ","-",df$DE)

# list all key terms and print the Top 200
tabTags <- tableTag(df, Tag = "DE")
print(tabTags[1:200])

# write all key terms into text file (to create dictionary)
write.table(tabTags[tabTags >= minOccur_export],
            "terms/keyTerms_author_raw.txt", sep = ";", row.names = FALSE, col.names = c("keyterm","count"))