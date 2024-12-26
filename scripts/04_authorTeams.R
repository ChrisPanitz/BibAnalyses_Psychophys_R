# --- author: Christian Panitz
# --- encoding: UTF-8
# --- R version: 4.3.1 (2023-06-16) -- "Beagle Scouts"
# --- RStudio version: 2023.06.0
# --- script version: Oct 2024
# --- content: Run analyses and plot results on authorship analyses (team size, gender of submitting authors, international co-authorships)


### header ###
yearFrom <- 1998 # for country data - 1998 is the first year without missing country data; no data for 1964-1972
yearTo <- 2023 # for country data

countries2plot <- 15 # top X countries to plot and report
minCollabEdges <- 10 # only plot links between countries with at least XX publications between them

fontSize <- 12 # for plot
### end header ###



# load packages
library(here)
library(bibliometrix)
library(tidyr)
library(stringr)
library(psych)
library(ggplot2)
library(igraph)
library(ggpubr)
library(gridGraphics)
library(flextable)

# parent folder
parentFolder <- here()

# load  bibliometric data frame
filename <- paste0(parentFolder, "/rawData/allPapers.rds")
df <- readRDS(filename)



#########################
### Number of authors ###
#########################

# create variable for number of listed authors on each publication and describe by year
df$nrAuthors <- lengths(str_split(df$AU, ";"))
describeBy(df$nrAuthors, group = df$PY)

# create vectors for mean and median number of authors by year and make quick plot
meanNrAuthors <- aggregate(nrAuthors ~ PY, data = df, FUN = "mean")
mdNrAuthors <- aggregate(nrAuthors ~ PY, data = df, FUN = "median")
plot(meanNrAuthors)
plot(mdNrAuthors)

# create data frame for table with mean/median number of authors by year
# structured for flextable to have to column groups
dfAuthors <- data.frame(
  year1 = as.integer(meanNrAuthors$PY[is.element(meanNrAuthors$PY,1964:1993)]),
  mean1 = round(meanNrAuthors$nrAuthors[is.element(meanNrAuthors$PY,1964:1993)],2),
  median1 = mdNrAuthors$nrAuthors[is.element(meanNrAuthors$PY,1964:1993)],
  empty = "",
  year2 = as.integer(meanNrAuthors$PY[is.element(meanNrAuthors$PY,1994:2023)]),
  mean2 = round(meanNrAuthors$nrAuthors[is.element(meanNrAuthors$PY,1994:2023)],2),
  median2 = mdNrAuthors$nrAuthors[is.element(meanNrAuthors$PY,1994:2023)]
)

# format flextable
authorTable <- flextable(dfAuthors)
authorTable <- colformat_num(authorTable, big.mark = "")
authorTable <- set_header_labels(authorTable, values = c("Year", "Mean", "Median", "", "Year", "Mean", "Median"))
authorTable <- align(authorTable, align = "center", part  = "all")
authorTable <- fontsize(authorTable, size = 10, part = "all")
authorTable <- font(authorTable, fontname = "Times New Roman", part = "all")
authorTable <- bold(authorTable, part = "header")
authorTable <- width(authorTable, width = 2, unit = "cm")
authorTable

# save flextable for author team size (mean + median)
save_as_docx(authorTable, path = paste0(parentFolder, "/tables/authorTeamSize.docx"))


### Plot for number of authors ###
# create dataframe where outlier gets value closer to the rest of the sample (==> broken y axis)
dfOutlierCapped <- df
dfOutlierCapped$nrAuthors[dfOutlierCapped$nrAuthors == max(dfOutlierCapped$nrAuthors)] <- 30

# make plot for number of authors by year, including single data points (goes to supplement)
teamsizePlot <- ggplot(data = dfOutlierCapped, aes(x = PY, group = PY, y = nrAuthors)) +
  geom_count(color = "black", alpha = .20) +
  stat_summary(aes(x = PY, y = nrAuthors), geom = "crossbar", fun = "median", color = "darkblue") +
  stat_summary(aes(x = PY, y = nrAuthors), geom = "point", fun = "mean", color = "darkred", size = 2) +
  geom_vline(xintercept = 1962) +
  geom_rect(xmin = 1961, xmax = 1963, ymin = 27, ymax = 28, fill = "white") +
  geom_segment(x = 1961, xend = 1963, y = 26.75, yend = 27.25) +
  geom_segment(x = 1961, xend = 1963, y = 27.75, yend = 28.25) +
  geom_point(aes(x = 1964, y = 29), color = "darkred", size = 2) +
  geom_text(aes(x = 1965, y = 29), label = "Mean", hjust = 0, vjust = 0.5) +
  geom_segment(aes(x = 1963.5, xend = 1964.5, y = 28, yend = 28), color = "darkblue", linewidth = 1) +
  geom_text(aes(x = 1965, y = 28), label = "Median", hjust = 0, vjust = 0.5) +
  scale_x_continuous(name = "Year", breaks = seq(1970,2020,10), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,30), breaks = seq(0,30,5), labels = c(0,5,10,15,20,25,83),
                     name = "Number of Authors", expand = c(0,0,0,2)) +
  coord_cartesian(clip = 'off') +
  labs(size = "# Publications:") +
  theme_classic() +
  theme(legend.position = c(0.21,0.97),
        legend.direction = "horizontal",
        legend.title = element_text(size = fontSize),
        legend.text = element_text(size = fontSize),
        axis.title = element_text(size = fontSize+2),
        axis.text = element_text(size = fontSize, color = "black"),
        axis.line.y = element_blank()); teamsizePlot

# save the plot
ggsave(paste0(parentFolder, "/plots/TeamSize.pdf"), teamsizePlot,
       width = 30, height = 20, units = "cm")

ggsave(paste0(parentFolder, "/plots/TeamSize.png"), teamsizePlot,
       width = 30, height = 20, units = "cm", dpi = 600)



### plot for the main manuscript, only with mean and median values (no single data points)
teamsizePlot_onlyMMd <- ggplot(data = dfOutlierCapped, aes(x = PY, group = PY, y = nrAuthors)) +
  stat_summary(aes(x = PY, y = nrAuthors), geom = "crossbar", fun = "median", color = "darkblue", alpha = .80) +
  stat_summary(aes(x = PY, y = nrAuthors), geom = "point", fun = "mean", color = "darkred", alpha = .80) +
  geom_point(aes(x = 1964, y = 5), color = "darkred", size = 2, alpha = .80) +
  geom_text(aes(x = 1965, y = 5), label = "Mean", hjust = 0, vjust = 0.5) +
  geom_segment(aes(x = 1963.5, xend = 1964.5, y = 4.9, yend = 4.9), color = "darkblue", linewidth = 1, alpha = .80) +
  geom_text(aes(x = 1965, y = 4.9), label = "Median", hjust = 0, vjust = 0.5) +
  scale_x_continuous(name = "Year of Publication", breaks = seq(1970,2020,10)) +
  scale_y_continuous(breaks = 2:5, minor_breaks = seq(2,5,0.1), name = "Number of Authors") +
  theme_classic() +
  theme(axis.text = element_text(size = fontSize - 2, color = "black"),
        axis.title = element_text(size = fontSize)); teamsizePlot_onlyMMd

# save the plot
ggsave(paste0(parentFolder, "/plots/TeamSize_onlyMMd.pdf"), teamsizePlot_onlyMMd,
       width = 15, height = 15, units = "cm")

ggsave(paste0(parentFolder, "/plots/TeamSize_onlyMMd.png"), teamsizePlot_onlyMMd,
       width = 15, height = 15, units = "cm", dpi = 600)



####################################
### gender of submitting authors ###
####################################

# load raw data
filename <- paste0(parentFolder, "/rawData/genderAuthors_Psychophysiology_1994to2023.txt")
dfGender <- read.csv(filename, sep = ";")

# show percentage of female submitting authors by year - since 2011 (no missing data since)
mean(dfGender$percentFemale[is.element(dfGender$year, 2011:2023)])
median(dfGender$percentFemale[is.element(dfGender$year, 2011:2023)])

# make plot for percentage of female submitting authors by year
yMaxGender <- 60
genderPlot <- ggplot(data = dfGender, aes(x = year, y = percentFemale)) +
  theme_classic() +
  geom_hline(yintercept = 50, color = "gray50", linetype = "dashed") +
  geom_line(color = "darkred", alpha = .30) + 
  geom_point(color = "darkred") + 
  geom_text(aes(y = percentFemale + yMaxGender/35, label = percentFemale), size = fontSize/3.5, fontface = "bold", color = "darkred") +
  scale_x_continuous(name = "Year", limits = c(1994,2023), 
                     breaks = seq(5*ceiling(min(dfGender$year)/5),max(dfGender$year),5),
                     minor_breaks = seq(1994,2023,1)) +
  scale_y_continuous(limits = c(0,yMaxGender), name = "% of Female First/Corresponding Authors", expand = c(0,0)) +
  theme(
    axis.title = element_text(size = fontSize + 2, color = "black"),
    axis.text = element_text(size = fontSize, color = "black")); genderPlot

# save the plot
ggsave(paste0(parentFolder, "/plots/percentFemaleAuthors.pdf"), genderPlot,
       width = 15, height = 15, units = "cm")

ggsave(paste0(parentFolder, "/plots/percentFemaleAuthors.png"), genderPlot,
       width = 15, height = 15, units = "cm", dpi = 600)



####################################
### international co-authorships ###
####################################

# extract country affiliations of authors
df <- metaTagExtraction(df, Field = "AU_CO")

# create variable with count of countries by year
df$nrCountries <- lengths(lapply(str_split(df$AU_CO, ";"), FUN = unique))
# compute mean and median number of countries by year (not reported in publication)
#meanNrCountries <- aggregate(nrCountries ~ PY, data = df, FUN = "mean")
#mdNrCountries <- aggregate(nrCountries ~ PY, data = df, FUN = "median")
# binary variable: publication with authors from at least 2 countries?
df$internatYesNo <- ceiling((df$nrCountries - 1.1) / 1000000)
# compute mean of binary variable and transorm it into %
#dfPercent <- aggregate(internatYesNo ~ PY, data = df, FUN = "mean")
dfPercent$internatYesNo <- round(100*dfPercent$internatYesNo,1)
# quick plots of the number of publishing countries by year and percentage of international collaborations by year 
#plot(meanNrCountries)
#plot(mdNrCountries)
plot(dfPercent)

# plot number and accumulated number of countries publishing in a given year 
#dfCountriesPerYear <- data.frame(
#  year = yearFrom:yearTo
#)
#for (i in 1:length(dfCountriesPerYear$year)){
#  dfCountriesPerYear$nrCountries[i] <- length(unique(unlist(str_split(df$AU_CO[df$PY == dfCountriesPerYear$year[i]], ";"))))
#  dfCountriesPerYear$nrCountriesCumulated[i] <- length(unique(unlist(str_split(df$AU_CO[df$PY <= dfCountriesPerYear$year[i]], ";"))))
#}
#plot(dfCountriesPerYear[,1:2])
#plot(dfCountriesPerYear[,c(1,3)])



#############################################
### plotting publication count by country ###
#############################################

# create dataframe with number of publication (per year and accumulated) by year
countryByYearWide <- KeywordGrowth(df[is.element(df$PY, yearFrom:yearTo),], Tag = "AU_CO", top = countries2plot)
countryByYear <- pivot_longer(countryByYearWide, cols = 2:(countries2plot+1), names_to = "country", values_to = "publications")
countryByYear$lineType <- factor(rep(c(rep(1,ceiling(countries2plot/2)), rep(2,floor(countries2plot/2))),
                                     dim(countryByYear)[1]/countries2plot))
# some formatting
countryByYear$country <- str_to_title(countryByYear$country)
countryByYear$country[countryByYear$country == "Usa"] <- "USA"
countryByYear$country[countryByYear$country == "United Kingdom"] <- "UK"

# table for total publication numbers (including some formatting)
totalCumulated <- data.frame(
  Country = str_to_title(names(countryByYearWide[-1])),
  Publications = as.numeric(countryByYearWide[dim(countryByYearWide)[1],-1])
)
totalCumulated$Country[totalCumulated$Country == "Usa"] <- "USA"
totalCumulated$Country[totalCumulated$Country == "United Kingdom"] <- "UK"

# create data frame for flextable
dfTable <- data.frame(
  country1 = totalCumulated$Country[1:5],
  pubs1 = totalCumulated$Publications[1:5],
  blank1 = rep(" ",5),
  country2 = totalCumulated$Country[6:10],
  pubs2 = totalCumulated$Publications[6:10],
  blank2 = rep(" ",5),
  country3 = totalCumulated$Country[11:15],
  pubs3 = totalCumulated$Publications[11:15]
)
# formatting flextable
countryTable <- flextable(data = dfTable)
countryTable <- set_header_labels(countryTable, values = c("Country", "Publications", " ", "Country", "Publications", " ", "Country", "Publications"))
countryTable <- align(countryTable, align = "center", part  = "all")
countryTable <- fontsize(countryTable, size = 10, part = "all")
countryTable <- font(countryTable, fontname = "Times New Roman", part = "all")
countryTable <- bold(countryTable, part = "header")
countryTable <- width(countryTable, width = 2.3, unit = "cm")
countryTable <- width(countryTable, j = c(3,6), width = 1, unit = "cm")

# save the table
save_as_docx(countryTable, path = paste0(parentFolder, "/tables/countryTableTop", countries2plot, ".docx"))



####################################################
### same thing with all countries for supplement ###
####################################################

# determine number of different countries publishing at least once
nrDiffCountries <- length(unique(unlist(str_split(df[is.element(df$PY, yearFrom:yearTo),"AU_CO"], ";"))))
# create dataframe with number of publication (per year and accumulated) by year
countryByYearAllWide <- KeywordGrowth(df[is.element(df$PY, yearFrom:yearTo),], Tag = "AU_CO", top = nrDiffCountries)


# table for total publication numbers
totalCumulatedAll <- data.frame(
  Country = str_to_title(names(countryByYearAllWide[-1])),
  Publications = as.numeric(countryByYearAllWide[dim(countryByYearAllWide)[1],-1])
)
# some formatting
totalCumulatedAll$Country[totalCumulatedAll$Country == "Usa"] <- "USA"
totalCumulatedAll$Country[totalCumulatedAll$Country == "United Kingdom"] <- "UK"
totalCumulatedAll$Country[totalCumulatedAll$Country == "Turkey"] <- "Türkiye"
totalCumulatedAll$Country[totalCumulatedAll$Country == "U Arab Emirates"] <- "United Arab Emirates"

# create data frame for flextable
dfTableAll <- data.frame(
  country1 = totalCumulatedAll$Country[1:ceiling(nrdiffCountries/3)],
  pubs1 = totalCumulatedAll$Publications[1:ceiling(nrdiffCountries/3)],
  blank1 = rep(" ",18),
  country2 = totalCumulatedAll$Country[(ceiling(nrDiffCountries/3)+1):(2*ceiling(nrDiffCountries/3))],
  pubs2 = totalCumulatedAll$Publications[(ceiling(nrDiffCountries/3)+1):(2*ceiling(nrDiffCountries/3))],
  blank2 = rep(" ",18),
  country3 = totalCumulatedAll$Country[(2*ceiling(nrDiffCountries/3)+1):nrDiffCountries],
  pubs3 = totalCumulatedAll$Publications[(2*ceiling(nrDiffCountries/3)+1):nrDiffCountries]
)
# formatting flextable
countryTableAll <- flextable(data = dfTableAll)
countryTableAll <- set_header_labels(countryTableAll, values = c("Country", "Publications", " ", "Country", "Publications", " ", "Country", "Publications"))
countryTableAll <- align(countryTableAll, align = "center", part  = "all")
countryTableAll <- fontsize(countryTableAll, size = 10, part = "all")
countryTableAll <- font(countryTableAll, fontname = "Times New Roman", part = "all")
countryTableAll <- bold(countryTableAll, part = "header")
countryTableAll <- width(countryTableAll, width = 2.3, unit = "cm")
countryTableAll <- width(countryTableAll, j = c(3,6), width = 1, unit = "cm")

# save the table
save_as_docx(countryTableAll, path = paste0(parentFolder, "/tables/countryTableAll.docx"))


#############################
### plotting country data ###
#############################

# create color map
colorVals <- rep(rainbow(n = ceiling(countries2plot/2), s = 1, v = 1.00, start = 0, end = 1-1/ceiling(countries2plot/2)),2)
colorValsDark <- rep(rainbow(n = ceiling(countries2plot/2), s = 1, v = 0.70, start = 0, end = 1-1/ceiling(countries2plot/2)),2)
colorVals[seq(1,length(colorVals),2)] <- colorValsDark[seq(1,length(colorVals),2)]
colorVals <- colorVals[1:countries2plot]

# transform USA data to different scale
countryByYear$publications[countryByYear$country == "USA"] <- countryByYear$publications[countryByYear$country == "USA"] / 2

# plot line plot for accmulated publications by country and year
timecoursePlot <- ggplot(countryByYear, aes(x = Year, y = publications, color = country)) +
  theme_classic() +
  geom_line(aes(linetype = country)) +
  scale_y_continuous(name = "Accumulated Number of Publications", sec.axis = sec_axis(trans =~ .*2, name = "Publications USA")) +
  scale_color_manual(values = colorVals, breaks = unique(countryByYear$country)) +
  scale_linetype_manual(values = c(rep("solid",ceiling(countries2plot/2)),
                                   rep("dashed",floor(countries2plot/2))),
                        breaks = unique(countryByYear$country)) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = fontSize - 2, color = "black"),
        axis.title = element_text(size = fontSize, color = "black"),
        axis.text = element_text(size = fontSize - 2, color = "black"),
        axis.text.y.right = element_text(color = colorVals[1]),
        axis.title.y.right = element_text(color = colorVals[1]),
        axis.line.y.right = element_line(color = colorVals[1]),
        axis.ticks.y.right = element_line(color = colorVals[1])); timecoursePlot

# save line plot for country publications
ggsave(paste0(parentFolder, "/plots/publicationsByCountry.pdf"), timecoursePlot,
       width = 15, height = 12, units = "cm")

ggsave(paste0(parentFolder, "/plots/publicationsByCountry.png"), timecoursePlot,
       width = 15, height = 12, units = "cm")



####################################################
### plot percentage international collaborations ###
####################################################

# create line plot
percentPlot <- ggplot(dfPercent[is.element(dfPercent$PY, yearFrom:yearTo),], aes(x = PY, y = internatYesNo)) +
  theme_classic() +
  stat_smooth(method = "loess", formula = y ~ x, geom = "smooth", se = FALSE, alpha = .50, color = "gray80") +
  geom_point(color = "darkred") +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "% of International Publications") +
  theme(axis.title = element_text(size = fontSize, color = "black"),
        axis.text = element_text(size = fontSize - 2, color = "black"),
        plot.margin = margin(t=10,r=35,b=10,l=10, unit = "mm")); percentPlot



###########################################
### international collaboration network ###
###########################################

# compute collaboration network
collabNetwork <- biblioNetwork(df[is.element(df$PY,yearFrom:yearTo),], analysis = "collaboration", network = "countries", sep = ";", n = countries2plot)
collabPlot <- networkPlot(collabNetwork, type = "circle", size = TRUE,
                          remove.multiple = FALSE, edges.min = minCollabEdges, Title = "")

# redo the vertex labels
V(collabPlot$graph)$label <- str_to_title(V(collabPlot$graph)$label)
V(collabPlot$graph)$label[V(collabPlot$graph)$label == "Usa"] <- "USA"
V(collabPlot$graph)$label[V(collabPlot$graph)$label == "United Kingdom"] <- "UK"

# overwrite colors to match with line plot
# normally, igraph assigns colors to clusters; however, here there were only 2 clusters: Japan (not sharing a link with any other countries) and the rest
V(collabPlot$graph)$color <- colorVals
V(collabPlot$graph)$frame.color <- NA
V(collabPlot$graph)$label.color <- "black"
V(collabPlot$graph)$label.dist <- 1.5
V(collabPlot$graph)$size <- V(collabPlot$graph)$size*2
E(collabPlot$graph)$color <- "gray50"
E(collabPlot$graph)$width <- E(collabPlot$graph)$width*5


### table for counts of international collaborations by country
# create data frame for flextable 
collabPlot4table <- networkPlot(collabNetwork, type = "circle", size = TRUE,
                                remove.multiple = FALSE, edges.min = 10, Title = "")
totalCollabs <- degree(collabPlot4table$graph) # total number of collaborations
collabMatDF <- as.data.frame(as.matrix(as_adjacency_matrix(collabPlot4table$graph)))
collabMatDF[collabMatDF == 0] <- "."
names(collabMatDF) <- str_to_title(names(collabMatDF))
collabMatDF$Country <- names(collabMatDF)
collabMatDF <- collabMatDF[c(countries2plot+1, 1:countries2plot)]
collabMatDF$Total <- totalCollabs

# some formatting
collabTable <- flextable(collabMatDF)
collabTable <- align(collabTable, align = "center", part  = "all")
collabTable

# and save the table
save_as_docx(collabTable, path = paste0(parentFolder, "/tables/collabTable_export.docx"))



##########################################################
### combine percentage and collaboration network plots ###
##########################################################

# plot percentage graph  and record it so it can be combined with the line plot
par(oma = c(1,1,1,1))
par(mar= c (1,1,1,1))
plot(collabPlot$graph)
collabPlotRec <- recordPlot()

# combine plots
combPlot <- ggarrange(percentPlot, collabPlotRec,
                      nrow = 1, ncol = 2,
                      widths = c(3,2),
                      labels = c("A","B")); combPlot

# and save them
ggsave(paste0(parentFolder, "/plots/internationalCollabs.pdf"), combPlot,
              width = 30, height = 12, units = "cm")
ggsave(paste0(parentFolder, "/plots/internationalCollabs.png"), combPlot,
       width = 30, height = 12, units = "cm", dpi = 600)