### header ###
splitYears <- c(1983, 2003) # upper edges of time windows (no need to define most recent year)
### end header ###



# load packages
library(here)
library(bibliometrix)

# parent folder
parentFolder <- here()

# load  bibliometric data frame
filename <- paste0(parentFolder, "/rawData/allPapers.rds")
df <- readRDS(filename)

# load prepared lists of terms to delete and synonyms
terms2delete <- read.table(paste0(parentFolder, "/terms/terms2delete_TI.txt"), sep = ";")[,1]
synonyms <- read.table(paste0(parentFolder, "/terms/synonyms_TI.txt"), sep = ";")[,1]

# load multi-word key terms to keep
terms2keep_TI <- read.table(paste0(parentFolder, "/terms/compoundTerms_TI_selected.txt"), sep = ";")[,1]

# extract keywords from article titles
df <- termExtraction(df, Field = "TI", ngrams = 1, remove.numbers = FALSE,
                     remove.terms = terms2delete, synonyms = synonyms, verbose = TRUE)

# custom way to treat multi-word terms as one term, words joint by hyphen
replace2keep_TI <- gsub(" ",";",terms2keep_TI)
replaceWith_TI <- gsub(" ","-",terms2keep_TI)
for (i in 1:length(terms2keep_TI)){
  df$TI_TM <- gsub(replace2keep_TI[i],replaceWith_TI[i],df$TI_TM)
}

# compute thematic evolution network...
evoNetwork <- thematicEvolution(df, field = "TI", years = splitYears, n = 250,
                                minFreq = 2, n.labels = 1, remove.terms = terms2delete,
                                synonyms = synonyms)

# ... and plot it
plotThematicEvolution(evoNetwork$Nodes, evoNetwork$Edges)
