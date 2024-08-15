### header ###
splitYears <- c(2001, 2012) # upper edges of time windows (no need to define most recent year)
### end header ###



# load packages
library(here)
library(bibliometrix)

# parent folder
parentFolder <- here()

# load  bibliometric data frame
filename <- paste0(parentFolder, "/rawData/allPapers.rds")
df <- readRDS(filename)

# there are no author keywords before 1991
df <- df[df$PY > 1990,]

# load prepared lists of terms to delete and synonyms
terms2delete <- read.table(paste0(parentFolder, "/terms/terms2delete_DE.txt"), sep = ";")[,1]
synonyms <- read.table(paste0(parentFolder, "/terms/synonyms_DE.txt"), sep = ";")[,1]


###
themMap <- thematicMap(df[is.element(df$PY, 2013:2023),], field = "DE", n = 250,
                       minfreq = 2, n.labels = 1, remove.terms = terms2delete,
                       synonyms = synonyms)

plot(themMap$map)
themMap$words[1:dim(themMap$words)[1],1:3]


###
evoNetwork <- thematicEvolution(df, field = "DE", years = splitYears, n = 250,
                                minFreq = 2, n.labels = 3, remove.terms = terms2delete,
                                synonyms = synonyms)

plotThematicEvolution(evoNetwork$Nodes, evoNetwork$Edges)
