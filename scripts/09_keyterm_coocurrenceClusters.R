# --- author: Christian Panitz
# --- encoding: UTF-8
# --- R version: 4.3.1 (2023-06-16) -- "Beagle Scouts"
# --- RStudio version: 2023.06.0
# --- script version: Oct 2024
# --- content: Compute and plot centrality and density for VOS Viewer clusters, write tables describing clusters


### header ###
fromYear <- 2014 # analyze publications from year XXXX
toYear <- 2023 # analyze publications until year XXXX

fontSize = 18# for plotting
### end header ###



# packages
library(here)
library(dplyr)
library(bibliometrix)
library(flextable)
library(igraph)
library(ggplot2)

# parent folder
parentFolder <- here()

# Load the VOS data from files
vosMap <- read.table(paste0(parentFolder,"/vosFiles/keyTerms_",fromYear,"to",toYear,"_map.txt"),
                     header = TRUE, sep = "\t")
vosNetwork <- read.table(paste0(parentFolder,"/vosFiles/keyTerms_",fromYear,"to",toYear,"_network.txt"),
                         header = FALSE, sep = "\t")
clustCol <- read.table(paste0(parentFolder,"/vosFiles/vosColors.txt"),
                       header = TRUE, sep = "\t")

# rename variables
names(vosNetwork) <- c("node1", "node2", "cooccurrences")

# Create igraph object from vos data
keyGraph <- graph_from_data_frame(vosNetwork, directed = FALSE)

# add cluster and label information
V(keyGraph)$cluster <- vosMap$cluster[match(V(keyGraph)$name, vosMap$id)]
V(keyGraph)$label <- vosMap$label[match(V(keyGraph)$name, vosMap$id)]

# create dataframe with cluster index, associated key terms, and number of key terms
graphDF <- summarize(group_by(vosMap, by = cluster), terms = paste(label, collapse = ", "))
names(graphDF) <- c("cluster", "keyterms")
graphDF$cluster <- factor(graphDF$cluster)
graphDF$clustSize <- as.vector(table(V(keyGraph)$cluster))
graphDF$keyterms <- gsub("-", " ", graphDF$keyterms)

# load co-occurrence data
coocMat <- as.matrix(read.table(file = paste0(parentFolder,"/vosFiles/keyTerms_",fromYear,"to",toYear,"_coocMat.txt"),
                      sep = ";", header = TRUE))

# compute total occurrences of key terms
totalOcc <- diag(coocMat)

# compute associative strength for all pairs of key terms (van Eck & Waltmann, 2014)
asMat <- 2 * sum(coocMat)/2 * coocMat /
  (outer(rowSums(coocMat), colSums(coocMat)))

# count key term occurrences, compute Callon centrality and density
graphDF$totalOccurrences <- NA
graphDF$centralityAS <- NA
graphDF$densityAS <- NA

for (clustI in 1:dim(graphDF)[1]){
  graphDF$totalOccurrences[clustI] <- sum(totalOcc[V(keyGraph)$cluster == clustI])
  graphDF$centralityAS[clustI] <- 10 * sum(asMat[V(keyGraph)$cluster == clustI, V(keyGraph)$cluster != clustI])
  graphDF$densityAS[clustI] <- 100 * sum(asMat[V(keyGraph)$cluster == clustI, V(keyGraph)$cluster == clustI]/2) /
                       sum(V(keyGraph)$cluster == clustI)
}



# create rank data for centrality and density; with minus because rank() ranks from lowest to highest
graphDF$centralityRank <- rank(-graphDF$centralityAS)
graphDF$densityRank <- rank(-graphDF$densityAS) 

# function to extract 5 most frequent key terms for each cluster
createTop5 <- function(terms) {
  splitTerms <- strsplit(terms, ",")[[1]] # Split the terms by commas
  splitTerms <- trimws(splitTerms) # Trim whitespaces
  selectedTerms <- splitTerms[1:min(5, length(splitTerms))] # Select the first 5 terms (or fewer in smaller clusters)
  paste(selectedTerms, collapse = "\n") # Combine them with line breaks
}

# Apply the function to graphDF
graphDF$top5 <- sapply(graphDF$keyterms, createTop5)

# read RGB codes for clusters and transform them into hex
clustCol$hexCol <- rgb(clustCol$red, clustCol$green, clustCol$blue, maxColorValue = 255)

# create data frame for table with descriptive cluster statistics
dfTable <- data.frame(
  cluster = graphDF$cluster,
  keyterms = graphDF$keyterms,
  clustSize = graphDF$clustSize,
  centralityAS = paste0(round(graphDF$centralityAS)," (",graphDF$centralityRank,")"),
  densityAS = paste0(round(graphDF$densityAS)," (",graphDF$densityRank,")")  
)

# create and format table
clusterTable <- flextable(dfTable)
clusterTable <- set_header_labels(clusterTable, values = c("Cluster", "Key Terms", "Size", "Centrality", "Density"))
clusterTable <- align(clusterTable, align = "center", part  = "all")
clusterTable <- fontsize(clusterTable, size = 10, part = "all")
clusterTable <- fontsize(clusterTable, size = 10, part = "all")
clusterTable <- font(clusterTable, fontname = "Times New Roman", part = "all")
clusterTable <- bold(clusterTable, part = "header")
clusterTable <- width(clusterTable, width = 2, unit = "cm")
clusterTable <- width(clusterTable, j = 2, width = 9, unit = "cm")

# set font colors to each cluster's color
for (rowI in 1:dim(dfTable)[1]){
  clusterTable <- color(clusterTable, i = rowI, color = clustCol$hexCol[rowI])
}

# show table
clusterTable

# save the table
save_as_docx(clusterTable, path = paste0(parentFolder, "/tables/keytermCoocurrences_",fromYear,"to",toYear,".docx"))



###############################################
### create plots for centrality and density ###
###############################################

### using original centrality and density data as used in the manuscript (and plotting the median values as lines)
cMin <- min(graphDF$centralityAS)
cMax <- max(graphDF$centralityAS)
cRan <- cMax - cMin
dMin <- min(graphDF$densityAS)
dMax <- max(graphDF$densityAS)
dRan <- dMax - dMin

# move word lists apart to avoid overlap when two clusters have similar centrality and density
graphDF$textX <- graphDF$centralityAS
for (i in 1:(max(graphDF$centralityRank)-1)){
  for (j in (i+1):max(graphDF$centralityRank)){
    xDist <- (graphDF$centralityAS[i]-graphDF$centralityAS[j]) / cRan
    yDist <- abs(graphDF$densityAS[i]-graphDF$densityAS[j]) / dRan
    if (xDist < 0.05 & xDist > 0 & yDist < 0.05){
      graphDF$textX[i] <- graphDF$textX[i] + cRan*0.07
      graphDF$textX[j] <- graphDF$textX[j] - cRan*0.07
    } else if (xDist > -0.05 & xDist < 0 & yDist < 0.05){
      graphDF$textX[i] <- graphDF$textX[i] - cRan*0.07
      graphDF$textX[j] <- graphDF$textX[j] + cRan*0.07
    }
  }
}

# plot it
themMapPlotMean <- ggplot(graphDF, aes(x = centralityAS, y = densityAS,
                                   color = cluster, size = totalOccurrences)) +
  theme_classic() +
  geom_vline(xintercept = mean(graphDF$centralityAS), linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = mean(graphDF$densityAS), linetype = "dashed", color = "gray50") +
  geom_point(alpha = .50) +
  geom_text(aes(x = textX, label = top5), size = fontSize/6, fontface = "bold", color ="black") +
  scale_x_continuous(name = "Centrality", breaks = NULL, limits = c(cMin-0.1*cRan,cMax+0.1*cRan)) +
  scale_y_continuous(name = "Density", breaks = NULL, limits = c(dMin-0.2*dRan,dMax+0.2*dRan)) +
  scale_color_manual(values = clustCol$hexCol) +
  scale_size_continuous(range = c(10,30)) +
  theme(legend.position = "none",
        axis.title = element_text(size = fontSize, color = "black"))
themMapPlotMean

# save plots
ggsave(paste0(parentFolder, "/plots/thematicMaps/themMap_Mean_",fromYear,"to",toYear,".pdf"), themMapPlotMean,
       width = 20, height = 20, units = "cm")
ggsave(paste0(parentFolder, "/plots/thematicMaps/themMap_Mean_",fromYear,"to",toYear,".png"), themMapPlotMean,
       width = 20, height = 20, units = "cm", dpi = 600)



### using ranked data (and plotting the median values as lines)
cMinMd <- min(graphDF$centralityRank)
cMaxMd <- max(graphDF$centralityRank)
dMinMd <- min(graphDF$densityRank)
dMaxMd <- max(graphDF$densityRank)

# plot it
themMapPlotMedian <- ggplot(graphDF, aes(x = cMaxMd+1-centralityRank, y = dMaxMd+1-densityRank,
                                         color = cluster, size = totalOccurrences)) +
  theme_classic() +
  geom_hline(yintercept = median(graphDF$centralityRank), linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = median(graphDF$densityRank), linetype = "dashed", color = "gray50") +
  geom_point(alpha = .70) +
  geom_text(aes(label = top5), size = fontSize/6, fontface = "bold", color ="black") +
  scale_x_continuous(name = "Centrality (Ranked)", breaks = NULL, limits = c(0,cMaxMd+1)) +
  scale_y_continuous(name = "Density (Ranked)", breaks = NULL, limits = c(0,dMaxMd+1)) +
  scale_color_manual(values = clustCol$hexCol) +
  scale_size_continuous(range = c(10,30)) +
  theme(legend.position = "none",
        axis.title = element_text(size = fontSize, color = "black"))
themMapPlotMedian

# save  plots
ggsave(paste0(parentFolder, "/plots/thematicMaps/themMap_Median_",fromYear,"to",toYear,".pdf"), themMapPlotMedian,
       width = 20, height = 20, units = "cm")
ggsave(paste0(parentFolder, "/plots/thematicMaps/themMap_Median_",fromYear,"to",toYear,".png"), themMapPlotMedian,
       width = 20, height = 20, units = "cm", dpi = 600)