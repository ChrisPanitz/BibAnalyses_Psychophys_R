### header ### 
minCite <- 1 # only include papers with at least [minCite] local citations when computing the network
nrVertices <- 50 # number of vertices to plot
### end header ###



# load packages
library(here)
library(bibliometrix)
library(tidyr)
library(stringr)
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


# Create data frame to export table of top publications
dfHistList <- data.frame(
  clustColor = histPlot$data$color_v,
  label = histPlot$data$id,
  localCit = histPlot$data$LCS,
  globalCit = histPlot$data$GCS,
  title = histPlot$data$title,
  doi = histPlot$data$DOI
)
dfHistList <- unique(dfHistList)

# some data structuring
clusterColors <- unique(dfHistList$clustColor)
dfHistList <- separate(dfHistList, col = label, into = c("firstAuthor","year"), sep = ",")
dfHistList$clustID <- factor(dfHistList$clustColor, levels = clusterColors, labels = 1:length(clusterColors))
dfHistList <- dfHistList[order(dfHistList$clustID, dfHistList$year),]

dfHistList$firstAuthor <- str_to_title(dfHistList$firstAuthor)
dfHistList$title <- str_to_title(dfHistList$title)
for (i in 1:dim(dfHistList)[1]){
  n <- nchar(dfHistList$firstAuthor[i])
  substr(dfHistList$firstAuthor[i],n-1,n) <- toupper(substr(dfHistList$firstAuthor[i],n-1,n))
}
dfHistList$firstAuthor[dfHistList$firstAuthor == "Duncanjohnson CC"] <- "Duncan-Johnson CC"
dfHistList$firstAuthor[dfHistList$firstAuthor == "Naatanen R"] <- "Näätänen R"

# creating and formatting the table
histTable <- flextable(data = dfHistList, col_keys = c("clustID", "year", "firstAuthor", "title", "localCit", "globalCit", "doi"))
histTable <- set_header_labels(histTable, values = c("Cluster", "Year", "First Author", "Title", "Local Citations", "Global Citations", "DOI"))
histTable <- align(histTable, align = "center", part  = "all")

for (rowI in 1:length(dfHistList$clustColor)){
  histTable <- color(histTable, i = rowI, color = dfHistList$clustColor[rowI])
}
histTable



#histPlot$data$y[histPlot$data$id == "obrist pa, 1970"] <- histPlot$data$y[histPlot$data$id == "obrist pa, 1970"] - 0.2
#histPlot$data$y[histPlot$data$id == "vogel ek, 2000"] <- histPlot$data$y[histPlot$data$id == "vogel ek, 2000"] + 0.2
#histPlot$data$yend[histPlot$data$id == "vogel ek, 2000"] <- histPlot$data$yend[histPlot$data$id == "vogel ek, 2000"] - 0.2
#histPlot$data$y[histPlot$data$id == "fridlund aj, 1986"] <- histPlot$data$y[histPlot$data$id == "fridlund aj, 1986"] - 0.05
#histPlot$data$yend[histPlot$data$id == "fridlund aj, 1986"] <- histPlot$data$yend[histPlot$data$id == "fridlund aj, 1986"] - 0.05

# move obrist 1970 a bit up
histPlot$data$y[histPlot$data$id == "obrist pa, 1970"] <- histPlot$data$y[histPlot$data$id == "obrist pa, 1970"] + 0.04
# move naatanen 1987 a bit up
histPlot$data$y[histPlot$data$id == "naatanen r, 1987"] <- histPlot$data$y[histPlot$data$id == "naatanen r, 1987"] + 0.03
histPlot$data$yend[histPlot$data$id == "naatanen r, 1987"] <- histPlot$data$yend[histPlot$data$id == "naatanen r, 1987"] + 0.03
histPlot$data$yend[histPlot$data$id == "vogel ek, 2000"] <- histPlot$data$yend[histPlot$data$id == "vogel ek, 2000"] + 0.03
# move bradley 1990 down a bit up
histPlot$data$y[histPlot$data$id == "bradley mm, 1990"] <- histPlot$data$y[histPlot$data$id == "bradley mm, 1990"] - 0.05
histPlot$data$yend[histPlot$data$id == "lang pj, 1993"] <- histPlot$data$yend[histPlot$data$id == "lang pj, 1993"] - 0.05
histPlot$data$yend[histPlot$data$id == "cuthbert bn, 1996"][2] <- histPlot$data$yend[histPlot$data$id == "cuthbert bn, 1996"][2] - 0.05
# move schupp 2000 a bit up
histPlot$data$y[histPlot$data$id == "schupp ht, 2000"] <- histPlot$data$y[histPlot$data$id == "schupp ht, 2000"] + 0.05
histPlot$data$yend[histPlot$data$id == "keil a, 2002"][3] <- histPlot$data$yend[histPlot$data$id == "keil a, 2002"][3] + 0.05
# move kiesel 2008 a bit down
histPlot$data$y[histPlot$data$id == "kiesel a, 2008"] <- histPlot$data$y[histPlot$data$id == "kiesel a, 2008"] - 0.05
# move folstein 2008 a bit up
histPlot$data$y[histPlot$data$id == "folstein jr, 2008"] <- histPlot$data$y[histPlot$data$id == "folstein jr, 2008"] + 0.03
histPlot$data$yend[histPlot$data$id == "holroyd cb, 2008"] <- histPlot$data$yend[histPlot$data$id == "holroyd cb, 2008"] + 0.03
# duncanjohnson ==> Duncan-Johnson
histPlot$data$id[histPlot$data$id == "duncanjohnson cc, 1977"] <- "Duncan-Johnson CC, 1977"
# naatannen ==> Näätänen
histPlot$data$id[histPlot$data$id == "naatanen r, 1987"] <- "Näätänen R, 1987"

#histPlot$data$yend[histPlot$data$id == "sherwood a, 1986"][2] <- histPlot$data$yend[histPlot$data$id == "sherwood a, 1986"][2] - 0.05
# move fridlund 1986 a bit up
# histPlot$data$y[histPlot$data$id == "fridlund aj, 1986"] <- histPlot$data$y[histPlot$data$id == "fridlund aj, 1986"] + 0.05
# histPlot$data$yend[histPlot$data$id == "fridlund aj, 1986"] <- histPlot$data$yend[histPlot$data$id == "fridlund aj, 1986"] + 0.05
# histPlot$data$yend[histPlot$data$id == "blumenthal td, 2005"][2] <- histPlot$data$yend[histPlot$data$id == "blumenthal td, 2005"][2] + 0.05
# histPlot$data$yend[histPlot$data$id == "bradley mm, 1990"] <- histPlot$data$yend[histPlot$data$id == "bradley mm, 1990"] + 0.05
# # move coles 1989 a tiny bit up
# histPlot$data$y[histPlot$data$id == "coles mgh, 1989"] <- histPlot$data$y[histPlot$data$id == "coles mgh, 1989"] + 0.01
# histPlot$data$yend[histPlot$data$id == "coles mgh, 1989"] <- histPlot$data$yend[histPlot$data$id == "coles mgh, 1989"] + 0.01
# histPlot$data$yend[histPlot$data$id == "picton tw, 2000"][2] <- histPlot$data$yend[histPlot$data$id == "picton tw, 2000"][2] + 0.01
# histPlot$data$yend[histPlot$data$id == "kok a, 2001"][4] <- histPlot$data$yend[histPlot$data$id == "kok a, 2001"][4] + 0.01
# histPlot$data$yend[histPlot$data$id == "miller j, 1998"] <- histPlot$data$yend[histPlot$data$id == "miller j, 1998"] + 0.01
# # move verleger 1997 a tiny bit up
# histPlot$data$y[histPlot$data$id == "verleger r, 1997"] <- histPlot$data$y[histPlot$data$id == "verleger r, 1997"] + 0.02
# histPlot$data$yend[histPlot$data$id == "folstein jr, 2008"][2] <- histPlot$data$yend[histPlot$data$id == "folstein jr, 2008"][2] + 0.02
# histPlot$data$yend[histPlot$data$id == "kiesel a, 2008"][4] <- histPlot$data$yend[histPlot$data$id == "kiesel a, 2008"][4] + 0.02
# histPlot$data$yend[histPlot$data$id == "kok a, 2001"][5] <- histPlot$data$yend[histPlot$data$id == "kok a, 2001"][5] + 0.02
# # move kiesel 2008 a tiny bit up
# histPlot$data$y[histPlot$data$id == "kiesel a, 2008"] <- histPlot$data$y[histPlot$data$id == "kiesel a, 2008"] + 0.02
# # move kok 2001 a tiny bit up
# histPlot$data$y[histPlot$data$id == "kok a, 2001"] <- histPlot$data$y[histPlot$data$id == "kok a, 2001"] + 0.01
# # move schupp 2000 a tiny bit up
# histPlot$data$y[histPlot$data$id == "schupp ht, 2000"] <- histPlot$data$y[histPlot$data$id == "schupp ht, 2000"] + 0.01
# histPlot$data$yend[histPlot$data$id == "schupp ht, 2000"] <- histPlot$data$yend[histPlot$data$id == "schupp ht, 2000"] + 0.01
# histPlot$data$yend[histPlot$data$id == "keil a, 2002"] <- histPlot$data$yend[histPlot$data$id == "keil a, 2002"] + 0.01

histPlot$data$id <- str_to_title(histPlot$data$id)
for (i in 1:length(histPlot$data$id)){
  commaPos <- unlist(gregexpr(',', histPlot$data$id[i]))
  substr(histPlot$data$id[i],commaPos-2,commaPos-1) <- toupper(substr(histPlot$data$id[i],commaPos-2,commaPos-1))
}

histPlot <- histPlot +
  theme_classic() +
  #scale_color_manual(values = clusterColors[c(2,4,9,3,8,6,1,5,7)], aesthetics = c("color","fill")) +
  scale_color_manual(values = clusterColors[c(2,4,3,8,7,1,5,6)], aesthetics = c("color","fill")) +
  coord_cartesian(clip = 'off') +
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
ggsave(paste0(parentFolder, "/plots/HistoricalDirectCitationNetwork.png"), histPlot,
       width = 20, height = 20, units = "cm", dpi = 600)

# save the table
save_as_docx(histTable, path = paste0(parentFolder, "/tables/histTable_export.docx"))


