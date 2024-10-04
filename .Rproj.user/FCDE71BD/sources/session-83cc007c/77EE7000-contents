### header ###
# for network analysis'
fromYear <- 1964
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

#
showEntries <- 10
showEntriesQuant <- 50

fontSize = 12
### end header ###



# load packages
library(here)
library(bibliometrix)
library(dplyr)
library(tidyr)
library(flextable)
library(ggplot2)

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
tableTag(df, Tag = "TI_TM", remove.terms = terms2delete, synonyms = synonyms)
#tableTag(df[df$PY > 2012,], Tag = "TI_TM", remove.terms = terms2delete, synonyms = synonyms)
#sum(df$PY > 2012)
#tableTag(df, Tag = "DE", remove.terms = terms2delete, synonyms = synonyms)


# for decades
termsByDec <- data.frame(
  years64to73 = tolower(names(tableTag(df[is.element(df$PY, 1964:1973),], Tag = "TI_TM",
                                       remove.terms = terms2delete, synonyms = synonyms)[1:showEntries])),
  years74to83 = tolower(names(tableTag(df[is.element(df$PY, 1974:1983),], Tag = "TI_TM",
                                       remove.terms = terms2delete, synonyms = synonyms)[1:showEntries])),
  years84to93 = tolower(names(tableTag(df[is.element(df$PY, 1984:1993),], Tag = "TI_TM",
                                       remove.terms = terms2delete, synonyms = synonyms)[1:showEntries])),
  years94to03 = tolower(names(tableTag(df[is.element(df$PY, 1994:2003),], Tag = "TI_TM",
                                       remove.terms = terms2delete, synonyms = synonyms)[1:showEntries])),
  years04to13 = tolower(names(tableTag(df[is.element(df$PY, 2004:2013),], Tag = "TI_TM",
                                       remove.terms = terms2delete, synonyms = synonyms)[1:showEntries])),
  years14to23 = tolower(names(tableTag(df[is.element(df$PY, 2014:2023),], Tag = "TI_TM",
                                       remove.terms = terms2delete, synonyms = synonyms)[1:showEntries])),
  allTime = tolower(names(tableTag(df[is.element(df$PY, 1964:2023),], Tag = "TI_TM",
                                   remove.terms = terms2delete, synonyms = synonyms)[1:showEntries]))
)
termsByDec <- data.frame(lapply(termsByDec, function(x) gsub("-", " ", x)))
termsByDec <- data.frame(lapply(termsByDec, function(x) gsub("eeg", "EEG", x)))
termsByDec <- data.frame(lapply(termsByDec, function(x) gsub("erp", "ERP", x)))
termsByDec <- data.frame(lapply(termsByDec, function(x) gsub("p300", "P300", x)))

termTable <- flextable(data = termsByDec)
termTable <- set_header_labels(termTable, values = c("1964-1973", "1974-1983", "1984-1993", "1994-2003", "2004-2013", "2014-2023", "all time"))
termTable <- align(termTable, align = "center", part  = "all")

# save the table
save_as_docx(termTable, path = paste0(parentFolder, "/tables/termsByDecadeTable_export.docx"))


# Top 10 of all time over time
#keytermByYearCumulated <- KeywordGrowth(df[is.element(df$PY, yearFrom:yearTo),], Tag = "AU_CO", top = countries2plot)
keytermByYearCumulated <- KeywordGrowth(df, Tag = "TI_TM", top = showEntries, remove.terms = terms2delete, synonyms = synonyms)
keytermByYear <- keytermByYearCumulated
#keytermByYear[,2:(showEntries+1)] <- apply(rbind(rep(0,showEntries), keytermByYear[,2:(showEntries+1)]), 2, diff)
keytermByYear <- pivot_longer(keytermByYear, cols = 2:(showEntries+1), names_to = "keyterm", values_to = "occurrences")
#keytermByYear$lineType <- factor(rep(c(rep(1,ceiling(countries2plot/2)), rep(2,floor(countries2plot/2))),
#                                     dim(keytermByYear)[1]/countries2plot))
keytermByYear$keyterm <- tolower(keytermByYear$keyterm)
keytermByYear$keyterm <- gsub("-"," ",keytermByYear$keyterm)
keytermByYear$keyterm[keytermByYear$keyterm == "erp"] <- "ERP"
keytermByYear$keyterm[keytermByYear$keyterm == "eeg"] <- "EEG"

# create color map
colorVals <- rep(rainbow(n = showEntries, s = 1, v = 1.00, start = 0, end = 1-1/showEntries),2)
colorValsDark <- rep(rainbow(n = showEntries, s = 1, v = 0.70, start = 0, end = 1-1/showEntries),2)
colorVals[seq(1,length(colorVals),2)] <- colorValsDark[seq(1,length(colorVals),2)]
#colorVals <- colorVals[1:countries2plot]

# plot line plot for publications by country and year (not accumulated)
timecoursePlot <- ggplot(keytermByYear, aes(x = Year, y = occurrences, color = keyterm)) +
  theme_classic() +
  #geom_line(aes(linetype = keyterm)) +
  geom_line() +
  labs(y = "Number of Occurrences") +
  scale_color_manual(values = colorVals, breaks = unique(keytermByYear$keyterm)) +
  scale_x_continuous(breaks = seq(1960,2020,10)) +
  #scale_linetype_manual(values = c(rep("solid",ceiling(countries2plot/2)),
  #                                 rep("dashed",floor(countries2plot/2))),
  #                      breaks = unique(countryByYear$country)) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = fontSize - 2, color = "black"),
        axis.title = element_text(size = fontSize, color = "black"),
        axis.text = element_text(size = fontSize - 2, color = "black")); timecoursePlot

ggsave(paste0(parentFolder, "/plots/keytermsTimecourse.pdf"), timecoursePlot,
       width = 15, height = 12, units = "cm")
ggsave(paste0(parentFolder, "/plots/keytermsTimecourse.png"), timecoursePlot,
       width = 15, height = 12, units = "cm", dpi = 600)



# plot median and quantiles of publication years for top key terms (bibliometrix original)
fieldByYear(df, field = "TI_TM", timespan = c(fromYear, toYear),
            min.freq = 5, n.items = 1, 
            remove.terms = terms2delete, synonyms = synonyms,
            graph = TRUE, dynamic.plot = TRUE)

### custom plot
A <- cocMatrix(df, Field = "TI_TM", binary = FALSE, remove.terms = terms2delete, synonyms = synonyms)
n <- colSums(as.array(A))

quants2plot <- c(0.25,0.50,0.75)
trend_med <- apply(A, 2, function(x) {   
  round(quantile(rep(df$PY, x), quants2plot, na.rm=TRUE))
})

trend_med <- as_tibble(t(trend_med)) %>% 
  rename("year_q1"=paste0(round(quants2plot[1]*100),"%"),
         "year_med"=paste0(round(quants2plot[2]*100),"%"),
         "year_q3"=paste0(round(quants2plot[3]*100),"%")) %>%  
  mutate(item=rownames(t(trend_med)), freq=n) %>% 
  #relocate(c(.data$item,.data$freq), .data$year_q1)
  relocate(c(item, freq), year_q1)

trendMed2plot <- trend_med[1:showEntriesQuant,]
trendMed2plot$item <- gsub("-"," ",trendMed2plot$item)
trendMed2plot$item <- tolower(trendMed2plot$item)
trendMed2plot$item[trendMed2plot$item == "erp"] <- "ERP"
trendMed2plot$item[trendMed2plot$item == "eeg"] <- "EEG"
trendMed2plot$item[trendMed2plot$item == "p300"] <- "P300"


quantYearsPlot <- ggplot(data = trendMed2plot, aes(x = item)) +
                    theme_classic() +
                    #geom_hline(yintercept = seq(1970,2020,10), color = "gray30", linetype = "solid") +
                    geom_rect(xmin = 0, xmax = 51, ymin = 1980, ymax = 1990, fill = "gray80", color = NA) +
                    geom_rect(xmin = 0, xmax = 51, ymin = 2000, ymax = 2010, fill = "gray80", color = NA) +
                    geom_rect(xmin = 0, xmax = 51, ymin = 2020, ymax = 2023, fill = "gray80", color = NA) +
                    geom_point(aes(y = year_med)) +
                    geom_errorbar(aes(ymin = year_q1, ymax = year_q3)) +
                    scale_x_discrete(limits = trendMed2plot$item[order(trendMed2plot$freq,decreasing = FALSE)],
                                     name = "Key Terms") +
                    scale_y_continuous(name = "Year") +
                    coord_flip() +
                    theme(axis.text = element_text(color = "black")); quantYearsPlot

ggsave(paste0(parentFolder,"/plots/keytermsQuantiles.pdf"), quantYearsPlot,
       width = 20,height = 30, unit = "cm")
ggsave(paste0(parentFolder,"/plots/keytermsQuantiles.png"), quantYearsPlot,
       width = 20,height = 30, unit = "cm", dpi = 600)
