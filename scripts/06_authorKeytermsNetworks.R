### header ###
# for network analysis'
fromYear <- 191
toYear <- 2001
nrNetworkItems <- NULL # how many items are included in the computation of the network (NULL = all available)
deleteSingleOccurrences <- TRUE # TRUE/FALSE: delete keyword that have a frequency of 1
shortLabels <- TRUE # TRUE/FALSE: plot short labels (1st author + year)

# for plotting
nrItems2plot <- 50
minOccurences <- NULL # has priority over nrItems2plot if not set to NULL
plotTitle <- ""
mapType <- "auto" # type of map layout; choose between "auto", "cirlce", "sphere", "mds", "fruchterman", and "kamada"
clusterMethod <- "walktrap" # method to cluster key terms; choose between "none", "optimal", "louvain","leiden", "infomap","edge_betweenness","walktrap", "spinglass", "leading_eigen", "fast_greedy"
vosPath <- "C:\\VosViewerJar" # path of VOS Viewer needed if plotting should be done by this software
removeIsolates <- TRUE # remove keywords that do not co-occur sufficiently with other keywords
minEdgeStrength <- 5
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

# standardize terms for GO NO-GO tasks
df$DE <- gsub(" GO;", " GO", df$DE)
df$DE <- gsub("NOGO", "NO-GO", df$DE)
df$DE <- gsub("GO NO-GO PARADIGM", "GO NO-GO", df$DE)
df$DE <- gsub("GO NO-GO TASK", "GO NO-GO", df$DE)



### TEMPORARY FOR ADDING WORDS TO DELETE ###
terms2delete <- c(terms2delete,
                  "deleteThisTerm")
############################################





# for descriptive stats: frequencies of the authors' key terms
tableTag(df, Tag = "DE", remove.terms = terms2delete, synonyms = synonyms)

# compute network
network_DE <- biblioNetwork(df[is.element(df$PY, fromYear:toYear),],
                             analysis = "co-occurrences",
                             network = "author_keywords",
                             n = NULL,
                             sep = ";",
                             short = deleteSingleOccurrences,
                             remove.terms = terms2delete,
                             synonyms = synonyms)

# ... and plot it
set.seed(1) # make the arrangement reproducible
nwPlot_DE <- networkPlot(network_DE,
                         normalize = NULL,
                         n = nrItems2plot,
                         degree = minOccurences,
                         Title = plotTitle,
                         type = mapType,
                         label = TRUE,
                         labelsize = 1,
                         label.cex = TRUE,
                         label.color = FALSE,
                         label.n = NULL,
                         halo = FALSE,
                         cluster = clusterMethod,
                         community.repulsion = 1,
                         vos.path = vosPath,
                         size = 3,
                         size.cex = TRUE,
                         curved = FALSE,
                         noloops = TRUE,
                         remove.multiple = TRUE,
                         remove.isolates = removeIsolates,
                         weighted = NULL,
                         edgesize = 1,
                         edges.min = minEdgeStrength,
                         alpha = 0.5,
                         verbose = TRUE)

net2VOSviewer(nwPlot_DE, vos.path = vosPath)
