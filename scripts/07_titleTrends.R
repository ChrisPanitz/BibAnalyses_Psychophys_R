# --- author: Christian Panitz
# --- encoding: UTF-8
# --- R version: 4.3.1 (2023-06-16) -- "Beagle Scouts"
# --- RStudio version: 2023.06.0
# --- script version: Oct 2024
# --- content: List top 10 key terms for each decade & all time; draw top 10 all-time over the years; create tables & plots



### header ###
showEntries <- 10 # plot top X key terms
showEntriesQuant <- 50 # for supplementary plot, showing median & quartiles of publication years for top XX key terms of all time

fontSize = 12 # for plotting

fromYear <- 1964 # for supplementary plot
toYear <- 2023 # for supplementary plot
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

# custom way to treat multi-word terms as one term, single words joint by hyphen
replaceWith_TI <- gsub(" ","-",terms2keep_TI)
for (i in 1:length(terms2keep_TI)){
  df$TI <- gsub(terms2keep_TI[i],replaceWith_TI[i],df$TI)
}

# extract keywords from article titles
df <- termExtraction(df, Field = "TI", ngrams = 1, remove.numbers = FALSE,
                     remove.terms = terms2delete, synonyms = synonyms, verbose = TRUE)



###########################
### key term statistics ###
###########################

# for descriptive statistics: frequencies of the key terms from article titles all-time
tableTag(df, Tag = "TI_TM", remove.terms = terms2delete, synonyms = synonyms)

# top key terms by decades & all time
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
# some manual editing
termsByDec <- data.frame(lapply(termsByDec, function(x) gsub("-", " ", x)))
termsByDec <- data.frame(lapply(termsByDec, function(x) gsub("eeg", "EEG", x)))
termsByDec <- data.frame(lapply(termsByDec, function(x) gsub("erp", "ERP", x)))
termsByDec <- data.frame(lapply(termsByDec, function(x) gsub("p300", "P300", x)))
termsByDec <- data.frame(lapply(termsByDec, function(x) gsub("scr", "SCR", x)))

# create & format flextable
termTable <- flextable(data = termsByDec)
termTable <- set_header_labels(termTable, values = c("1964-1973", "1974-1983", "1984-1993", "1994-2003", "2004-2013", "2014-2023", "all time"))
termTable <- align(termTable, align = "center", part  = "all")
termTable <- font(termTable, font = "Times New Roman", part = "all")
termTable <- fontsize(termTable, size = 10, part = "all")
termTable <- bold(termTable, part = "header")
termTable <- width(termTable, width = 2.5, unit = "cm")

# save the table
save_as_docx(termTable, path = paste0(parentFolder, "/tables/termsByDecadeTable_export.docx"))



####################################################
### plot all time top 10 over years (cumulative) ###
####################################################

# extract cumulative key term counts over years and creat data frame in long format
keytermByYear <- KeywordGrowth(df, Tag = "TI_TM", top = showEntries, remove.terms = terms2delete, synonyms = synonyms)
keytermByYear <- pivot_longer(keytermByYear, cols = 2:(showEntries+1), names_to = "keyterm", values_to = "occurrences")

# some manual editing
keytermByYear$keyterm <- tolower(keytermByYear$keyterm)
keytermByYear$keyterm <- gsub("-"," ",keytermByYear$keyterm)
keytermByYear$keyterm[keytermByYear$keyterm == "erp"] <- "ERP"
keytermByYear$keyterm[keytermByYear$keyterm == "eeg"] <- "EEG"

# create color map
colorVals <- rep(rainbow(n = showEntries, s = 1, v = 1.00, start = 0, end = 1-1/showEntries),2)
colorValsDark <- rep(rainbow(n = showEntries, s = 1, v = 0.70, start = 0, end = 1-1/showEntries),2)
colorVals[seq(1,length(colorVals),2)] <- colorValsDark[seq(1,length(colorVals),2)]

# plot line plot for cumulative publications by country and year
timecoursePlot <- ggplot(keytermByYear, aes(x = Year, y = occurrences, color = keyterm)) +
  theme_classic() +
  geom_line() +
  labs(y = "Number of Occurrences") +
  scale_color_manual(values = colorVals, breaks = unique(keytermByYear$keyterm)) +
  scale_x_continuous(breaks = seq(1960,2020,10)) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = fontSize - 2, color = "black"),
        axis.title = element_text(size = fontSize, color = "black"),
        axis.text = element_text(size = fontSize - 2, color = "black")); timecoursePlot

# save the plot
ggsave(paste0(parentFolder, "/plots/keytermsTimecourse.pdf"), timecoursePlot,
       width = 15, height = 12, units = "cm")
ggsave(paste0(parentFolder, "/plots/keytermsTimecourse.png"), timecoursePlot,
       width = 15, height = 12, units = "cm", dpi = 600)



##################################
### Supplement: Quantile plots ###
##################################
# plot median and quantiles of publication years for top key terms (bibliometrix function)
fieldByYear(df, field = "TI_TM", timespan = c(fromYear, toYear),
            min.freq = 5, n.items = 1, 
            remove.terms = terms2delete, synonyms = synonyms,
            graph = TRUE, dynamic.plot = TRUE)

### code copied and adapted from bibliometrix function
# count key term occurrences
A <- cocMatrix(df, Field = "TI_TM", binary = FALSE, remove.terms = terms2delete, synonyms = synonyms)
n <- colSums(as.array(A))

# compute quantiles for publication years by key term
quants2plot <- c(0.25,0.50,0.75)
trend_med <- apply(A, 2, function(x) {   
  round(quantile(rep(df$PY, x), quants2plot, na.rm=TRUE))
})

trend_med <- as_tibble(t(trend_med)) %>% 
  rename("year_q1"=paste0(round(quants2plot[1]*100),"%"),
         "year_med"=paste0(round(quants2plot[2]*100),"%"),
         "year_q3"=paste0(round(quants2plot[3]*100),"%")) %>%  
  mutate(item=rownames(t(trend_med)), freq=n) %>% 
  relocate(c(item, freq), year_q1)

# some manual editing
trendMed2plot <- trend_med[1:showEntriesQuant,]
trendMed2plot$item <- gsub("-"," ",trendMed2plot$item)
trendMed2plot$item <- tolower(trendMed2plot$item)
trendMed2plot$item[trendMed2plot$item == "erp"] <- "ERP"
trendMed2plot$item[trendMed2plot$item == "eeg"] <- "EEG"
trendMed2plot$item[trendMed2plot$item == "p300"] <- "P300"

# plot it
quantYearsPlot <- ggplot(data = trendMed2plot, aes(x = item)) +
                    theme_classic() +
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

# and save it
ggsave(paste0(parentFolder,"/plots/keytermsQuantiles.pdf"), quantYearsPlot,
       width = 20,height = 30, unit = "cm")
ggsave(paste0(parentFolder,"/plots/keytermsQuantiles.png"), quantYearsPlot,
       width = 20,height = 30, unit = "cm", dpi = 600)