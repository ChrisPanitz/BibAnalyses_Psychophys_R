### header ###
yearFrom <- 1998 # 1998 is the first year without missing country data; no data for 1964-1972
yearTo <- 2023
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

# load prepared lists of terms to delete and synonyms
terms2delete <- read.table(paste0(parentFolder, "/terms/terms2delete_DE.txt"), sep = ";")[,1]
synonyms <- read.table(paste0(parentFolder, "/terms/synonyms_DE.txt"), sep = ";")[,1]

# extract country affiliations of authors
df <- metaTagExtraction(df, Field = "AU_CO")

# compute network
countryNetwork <- biblioNetwork(df[is.element(df$PY, yearFrom:yearTo),],
                                analysis = "collaboration", network = "countries")

# and plot it
countryPlot <- networkPlot(countryNetwork, n = dim(countryNetwork)[1],
                           Title = "Country Collaboration", type = "circle",
                           size=TRUE, remove.multiple=FALSE,
                           labelsize=0.7,cluster="none")