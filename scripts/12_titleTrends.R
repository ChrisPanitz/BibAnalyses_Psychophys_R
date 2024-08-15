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
### end header ###



# load packages
library(here)
library(bibliometrix)
library(dplyr)
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
terms2delete <- c(terms2delete,
                  "individual",
                  "difference",
                  "differences")
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

# for descriptive statistics: frequencies of the key terms from article titles
tableTag(df, Tag = "TI_TM", remove.terms = terms2delete, synonyms = synonyms)
tableTag(df[df$PY > 2012,], Tag = "TI_TM", remove.terms = terms2delete, synonyms = synonyms)
sum(df$PY > 2012)
tableTag(df, Tag = "DE", remove.terms = terms2delete, synonyms = synonyms)


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


# plot median and quantiles of publication years for top key terms (bibliometrix original)
fieldByYear(df, field = "TI_TM", timespan = c(fromYear, toYear),
            min.freq = 5, n.items = 1, 
            remove.terms = terms2delete, synonyms = synonyms,
            graph = TRUE, dynamic.plot = TRUE)

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
  relocate(c(.data$item,.data$freq), .data$year_q1)

quantYearsPlot <- ggplot(data = trend_med[1:showEntriesQuant,], aes(x = item)) +
                         geom_point(aes(y = year_med)) +
                         geom_errorbar(aes(ymin = year_q1, ymax = year_q3)) +
                         scale_x_discrete(limits = trend_med$item[order(trend_med$freq[1:showEntriesQuant],decreasing = FALSE)],
                                          name = "Key terms") +
                         scale_y_continuous(name = "Year") +
                         coord_flip()
quantYearsPlot

ggsave(paste0(parentFolder,"/plots/quantilesYears_TI.pdf"), quantYearsPlot)
