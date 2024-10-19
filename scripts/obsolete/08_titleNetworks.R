### header ###
# for network analysis
fromYear <- 2004
toYear <- 2023
nrNetworkItems <- NULL # how many items are included in the computation of the network (NULL = all available)
deleteSingleOccurrences <- TRUE # TRUE/FALSE: delete keywords that have a frequency of 1
#shortLabels <- TRUE # TRUE/FALSE: plot short labels (1st author + year)

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

# load multi-word key terms to keep
terms2keep_TI <- read.table(paste0(parentFolder, "/terms/compoundTerms_TI_selected.txt"), sep = ";")[,1]

# load prepared lists of terms to delete and synonyms
terms2delete <- read.table(paste0(parentFolder, "/terms/terms2delete_TI.txt"), sep = ";")[,1]
synonyms <- read.table(paste0(parentFolder, "/terms/synonyms_TI.txt"), sep = ";")[,1]






### TEMPORARY FOR ADDING WORDS TO DELETE ###
#terms2delete <- c(terms2delete,
#                  "individual",
#                  "difference",
#                  "differences")
############################################







# extract keywords from article titles
df <- termExtraction(df, Field = "TI", ngrams = 1, remove.numbers = FALSE,
                     remove.terms = terms2delete, synonyms = synonyms, verbose = TRUE)

# custom way to treat multi-word terms as one term, words joint by hyphen
replace2keep_TI <- gsub(" ",";",terms2keep_TI)
replaceWith_TI <- gsub(" ","-",terms2keep_TI)
for (i in 1:length(terms2keep_TI)){
  df$TI_TM <- gsub(replace2keep_TI[i],replaceWith_TI[i],df$TI_TM)
}

# IND DIFF
df$TI_TM <- gsub("INDIVIDUAL;DIFFERENCE","INDIVIDUAL-DIFFERENCE",df$TI_TM)
df$TI_TM <- gsub("DIFFERENCE;INDIVIDUAL","INDIVIDUAL-DIFFERENCE",df$TI_TM)

# for descriptive statistics: frequencies of the key terms from article titles
tableTag(df, Tag = "TI_TM")

# compute network
network_TI <- biblioNetwork(df[is.element(df$PY, fromYear:toYear),],
                             analysis = "co-occurrences",
                             network = "titles",
                             n = 50,
                             sep = ";",
                             short = FALSE,#deleteSingleOccurrences,
                             remove.terms = terms2delete,
                             synonyms = synonyms)

# ... and plot it
set.seed(1) # make the arrangement reproducible
nwPlot_TI <- networkPlot(network_TI,
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
                         community.repulsion = 0.1,
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

net2VOSviewer(nwPlot_TI, vos.path = vosPath)
