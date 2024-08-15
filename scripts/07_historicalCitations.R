### header ### 
minCite <- 1 # only include papers with at least [minCite] local citations when computing the network
nrVertices <- 50 # number of vertices to plot
### end header ###



# load packages
library(here)
library(bibliometrix)
library(tidyr)
library(flextable)
library(ggplot2)

# parent folder
parentFolder <- here()

# load  bibliometric data frame
filename <- paste0(parentFolder, "/rawData/allPapers.rds")
df <- readRDS(filename)

# compute historical direct citation network
histResults <- histNetwork(df, min.citations = minCite)

# ... and plot it
set.seed(6); histPlot(histResults, n = nrVertices,
                      size = 3, labelsize = 3,
                      label = "short", remove.isolates = TRUE)
histPlot <- last_plot()

#clusterColorsRev <- clusterColors[seq(9,1,-1)]
#test <- c("#999999","#999999","#999999","#999999","#999999","#999999","#999999","#999999","#999999")
#histPlot$data$color_v <- rep("#999999",85)
#histPlot <- histPlot + scale_color_discrete(test) +  scale_fill_discrete(test)
#ggsave(filename = paste0(parentFolder, "/plots/histPlot.pdf"), plot = histPlot,
#       width = 20, height = 20, units = "cm")

# Create data frame to export table of top publications
dfHistList <- data.frame(
  clustColor = histPlot$data$color_v,
  label = histPlot$data$id,
  localCit = histPlot$data$LCS,
  globalCit = histPlot$data$GCS,
  title = histPlot$data$title
)
dfHistList <- unique(dfHistList)

# some data structuring
clusterColors <- unique(dfHistList$clustColor)
dfHistList <- separate(dfHistList, col = label, into = c("firstAuthor","year"), sep = ",")
dfHistList$clustID <- factor(dfHistList$clustColor, levels = clusterColors, labels = 1:length(clusterColors))
dfHistList <- dfHistList[order(dfHistList$clustID, dfHistList$year),]

# creating and formatting the table
histTable <- flextable(data = dfHistList, col_keys = c("clustID", "year", "firstAuthor", "title", "localCit", "globalCit", "doi"))
histTable <- set_header_labels(histTable, values = c("Cluster", "Year", "First Author", "Title", "Local Citations", "Global Citations", "DOI"))
histTable <- align(histTable, align = "center", part  = "all")

# for (colorI in 1:length(clusterColors)){
#   histTable <- color(histTable, i = dfHistList$clustID == colorI, color = clusterColors[colorI])
# }
for (rowI in 1:length(dfHistList$clustColor)){
  histTable <- color(histTable, i = rowI, color = dfHistList$clustColor[rowI])
}
histTable



#histPlot$data$y[histPlot$data$id == "obrist pa, 1970"] <- histPlot$data$y[histPlot$data$id == "obrist pa, 1970"] - 0.2
#histPlot$data$y[histPlot$data$id == "vogel ek, 2000"] <- histPlot$data$y[histPlot$data$id == "vogel ek, 2000"] + 0.2
#histPlot$data$yend[histPlot$data$id == "vogel ek, 2000"] <- histPlot$data$yend[histPlot$data$id == "vogel ek, 2000"] - 0.2
#histPlot$data$y[histPlot$data$id == "fridlund aj, 1986"] <- histPlot$data$y[histPlot$data$id == "fridlund aj, 1986"] - 0.05
#histPlot$data$yend[histPlot$data$id == "fridlund aj, 1986"] <- histPlot$data$yend[histPlot$data$id == "fridlund aj, 1986"] - 0.05

# move obrist 1978 a bit down
histPlot$data$y[histPlot$data$id == "obrist pa, 1978"] <- histPlot$data$y[histPlot$data$id == "obrist pa, 1978"] - 0.05
histPlot$data$yend[histPlot$data$id == "sherwood a, 1986"][2] <- histPlot$data$yend[histPlot$data$id == "sherwood a, 1986"][2] - 0.05
# move fridlund 1986 a bit up
histPlot$data$y[histPlot$data$id == "fridlund aj, 1986"] <- histPlot$data$y[histPlot$data$id == "fridlund aj, 1986"] + 0.05
histPlot$data$yend[histPlot$data$id == "fridlund aj, 1986"] <- histPlot$data$yend[histPlot$data$id == "fridlund aj, 1986"] + 0.05
histPlot$data$yend[histPlot$data$id == "blumenthal td, 2005"][2] <- histPlot$data$yend[histPlot$data$id == "blumenthal td, 2005"][2] + 0.05
histPlot$data$yend[histPlot$data$id == "bradley mm, 1990"] <- histPlot$data$yend[histPlot$data$id == "bradley mm, 1990"] + 0.05
# move coles 1989 a tiny bit up
histPlot$data$y[histPlot$data$id == "coles mgh, 1989"] <- histPlot$data$y[histPlot$data$id == "coles mgh, 1989"] + 0.01
histPlot$data$yend[histPlot$data$id == "coles mgh, 1989"] <- histPlot$data$yend[histPlot$data$id == "coles mgh, 1989"] + 0.01
histPlot$data$yend[histPlot$data$id == "picton tw, 2000"][2] <- histPlot$data$yend[histPlot$data$id == "picton tw, 2000"][2] + 0.01
histPlot$data$yend[histPlot$data$id == "kok a, 2001"][4] <- histPlot$data$yend[histPlot$data$id == "kok a, 2001"][4] + 0.01
histPlot$data$yend[histPlot$data$id == "miller j, 1998"] <- histPlot$data$yend[histPlot$data$id == "miller j, 1998"] + 0.01
# move verleger 1997 a tiny bit up
histPlot$data$y[histPlot$data$id == "verleger r, 1997"] <- histPlot$data$y[histPlot$data$id == "verleger r, 1997"] + 0.02
histPlot$data$yend[histPlot$data$id == "folstein jr, 2008"][2] <- histPlot$data$yend[histPlot$data$id == "folstein jr, 2008"][2] + 0.02
histPlot$data$yend[histPlot$data$id == "kiesel a, 2008"][4] <- histPlot$data$yend[histPlot$data$id == "kiesel a, 2008"][4] + 0.02
histPlot$data$yend[histPlot$data$id == "kok a, 2001"][5] <- histPlot$data$yend[histPlot$data$id == "kok a, 2001"][5] + 0.02
# move kiesel 2008 a tiny bit up
histPlot$data$y[histPlot$data$id == "kiesel a, 2008"] <- histPlot$data$y[histPlot$data$id == "kiesel a, 2008"] + 0.02
# move kok 2001 a tiny bit up
histPlot$data$y[histPlot$data$id == "kok a, 2001"] <- histPlot$data$y[histPlot$data$id == "kok a, 2001"] + 0.01
# move schupp 2000 a tiny bit up
histPlot$data$y[histPlot$data$id == "schupp ht, 2000"] <- histPlot$data$y[histPlot$data$id == "schupp ht, 2000"] + 0.01
histPlot$data$yend[histPlot$data$id == "schupp ht, 2000"] <- histPlot$data$yend[histPlot$data$id == "schupp ht, 2000"] + 0.01
histPlot$data$yend[histPlot$data$id == "keil a, 2002"] <- histPlot$data$yend[histPlot$data$id == "keil a, 2002"] + 0.01


histPlot <- histPlot +
  theme_classic() +
  scale_color_manual(values = clusterColors[c(2,4,9,3,8,6,1,5,7)], aesthetics = c("color","fill")) +
  theme(plot.title = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(), 
        axis.line = element_blank())

histPlot$layers[[4]] <- NULL # delete bibliometrix logo

histPlot

# save the plot
ggsave(paste0(parentFolder, "/plots/HistoricalDirectCitationNetwork.pdf"), histPlot,
       width = 20, height = 20, units = "cm")

# save the table
save_as_docx(histTable, path = paste0(parentFolder, "/tables/histTable_export.docx"))

