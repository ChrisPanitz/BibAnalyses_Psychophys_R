### header ###
fromYear <-2014
toYear <- 2023
nrItems2plot <- 50
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

# load multi-word key terms to keep
terms2keep_TI <- read.table(paste0(parentFolder, "/terms/compoundTerms_TI_selected.txt"), sep = ";")[,1]

# load prepared lists of terms to delete and synonyms
terms2delete <- read.table(paste0(parentFolder, "/terms/terms2delete_TI.txt"), sep = ";")[,1]
synonyms <- read.table(paste0(parentFolder, "/terms/synonyms_TI.txt"), sep = ";")[,1]

# extract keywords from article titles
df <- termExtraction(df, Field = "TI", ngrams = 1, remove.numbers = FALSE,
                     remove.terms = terms2delete, synonyms = synonyms, verbose = TRUE)

# custom way to treat multi-word terms as one term, words joint by hyphen
replace2keep_TI <- gsub(" ",";",terms2keep_TI)
replaceWith_TI <- gsub(" ","-",terms2keep_TI)
for (i in 1:length(terms2keep_TI)){
  df$TI_TM <- gsub(replace2keep_TI[i],replaceWith_TI[i],df$TI_TM)
}

# manually count +1 for individual difference(s) if individual and difference
# occur after each other, possibly with filler words in between
df$TI_TM <- gsub("INDIVIDUAL;DIFFERENCE","INDIVIDUAL-DIFFERENCE",df$TI_TM)
df$TI_TM <- gsub("DIFFERENCE;INDIVIDUAL","INDIVIDUAL-DIFFERENCE",df$TI_TM)

# extract occurrence matrix (binary; publication x keyword)
occMat <- cocMatrix(
  df[is.element(df$PY,fromYear:toYear),],
  Field = "TI_TM",
  type = "matrix",
  n = nrItems2plot,
  sep = ";",
  binary = FALSE,
  short = TRUE,
  remove.terms = terms2delete,
  synonyms = synonyms
)

# compute co-occurrence matrix (count data; keyword x keyword)
occMat[occMat > 0] <- 1 # some terms might have been double-counted because of synonyms
coocMat <- t(occMat) %*% occMat

# Get the indices of the upper triangle (excluding diagonal)
upper_tri_indices <- which(lower.tri(coocMat), arr.ind = TRUE)

# Filter out pairs where the co-occurrence count is 0
filtered_indices <- upper_tri_indices[coocMat[upper_tri_indices] > 0, ]

# Create the data frame with node1, node2, and co-occurrences
coocDF <- data.frame(
  node1 = filtered_indices[, 2],
  node2 = filtered_indices[, 1],
  cooccurrences = coocMat[filtered_indices]
)

# Create data frame with node labels
labelDF <- data.frame(
  id = 1:nrItems2plot,
  label = tolower(row.names(coocMat))
)
labelDF$label <- gsub("-", " ", labelDF$label)

# Save data
write.table(coocMat, file = paste0(parentFolder,"/vosFiles/keyTerms_",fromYear,"to",toYear,"_coocMat.txt"),
            sep = ";", col.names = TRUE, row.names = TRUE)

write.table(coocDF, file = paste0(parentFolder,"/vosFiles/keyTerms_",fromYear,"to",toYear,"_network.txt"),
            sep = "\t", col.names = FALSE, row.names = FALSE)

write.table(labelDF, file = paste0(parentFolder,"/vosFiles/keyTerms_",fromYear,"to",toYear,"_labels.txt"),
            sep = "\t", quote = FALSE, col.names = TRUE, row.names = FALSE)

