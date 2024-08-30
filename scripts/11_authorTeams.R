### header ###
yearFrom <- 1998 # 1998 is the first year without missing country data; no data for 1964-1972
yearTo <- 2023
### end header ###



# load packages
library(here)
library(bibliometrix)
library(tidyr)
library(stringr)
library(psych)
library(ggplot2)

# parent folder
parentFolder <- here()

# load  bibliometric data frame
filename <- paste0(parentFolder, "/rawData/allPapers.rds")
df <- readRDS(filename)

df$nrAuthors <- lengths(str_split(df$AU, ";"))
describeBy(df$nrAuthors, group = df$PY)
meanNrAuthors <- aggregate(nrAuthors ~ PY, data = df, FUN = "mean")
mdNrAuthors <- aggregate(nrAuthors ~ PY, data = df, FUN = "median")
plot(meanNrAuthors)
plot(mdNrAuthors)

##
dfOutlierCapped <- df
dfOutlierCapped$nrAuthors[dfOutlierCapped$nrAuthors == max(dfOutlierCapped$nrAuthors)] <- 30


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
  scale_y_continuous(limits = c(0,30), breaks = seq(0,30,5), minor_breaks = 0:30, labels = c(0,5,10,15,20,25,83), name = "Number of Authors", expand = c(0,0,0,2)) +
  coord_cartesian(clip = 'off') +
  labs(size = "# publications:") +
  theme_classic() +
  theme(legend.position = c(0.16,0.97),
        legend.direction = "horizontal",
        #legend.box.background = element_rect(linewidth = 1, color = "black"),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        axis.line.y = element_blank()); teamsizePlot

# save the plot
ggsave(paste0(parentFolder, "/plots/TeamSize.pdf"), teamsizePlot,
       width = 30, height = 20, units = "cm")

teamsizePlot_onlyMMd <- ggplot(data = dfOutlierCapped, aes(x = PY, group = PY, y = nrAuthors)) +
  stat_summary(aes(x = PY, y = nrAuthors), geom = "crossbar", fun = "median", color = "darkblue") +
  stat_summary(aes(x = PY, y = nrAuthors), geom = "point", fun = "mean", color = "darkred") +
  geom_point(aes(x = 1964, y = 5), color = "darkred", size = 2) +
  geom_text(aes(x = 1965, y = 5), label = "Mean", hjust = 0, vjust = 0.5) +
  geom_segment(aes(x = 1963.5, xend = 1964.5, y = 4.9, yend = 4.9), color = "darkblue", linewidth = 1) +
  geom_text(aes(x = 1965, y = 4.9), label = "Median", hjust = 0, vjust = 0.5) +
  scale_x_continuous(name = "Year of Publication", breaks = seq(1970,2020,10)) +
  scale_y_continuous(breaks = 2:5, minor_breaks = seq(2,5,0.1), name = "Number of Authors") +
  theme_classic(); teamsizePlot_onlyMMd

ggsave(paste0(parentFolder, "/plots/TeamSize_onlyMMd.pdf"), teamsizePlot_onlyMMd,
       width = 20, height = 20, units = "cm")



# extract country affiliations of authors
df <- metaTagExtraction(df, Field = "AU_CO")


df$nrCountries <- lengths(lapply(str_split(df$AU_CO, ";"), FUN = unique))
meanNrCountries <- aggregate(nrCountries ~ PY, data = df, FUN = "mean")
mdNrCountries <- aggregate(nrCountries ~ PY, data = df, FUN = "median")
plot(meanNrCountries)
plot(mdNrCountries)

max(df$nrAuthors)
# number of countries total in a given year
dfCountriesPerYear <- data.frame(
  #year = yearFrom:yearTo
  year = 1964:yearTo
)
for (i in 1:length(dfCountriesPerYear$year)){
  dfCountriesPerYear$nrCountries[i] <- length(unique(unlist(str_split(df$AU_CO[df$PY == dfCountriesPerYear$year[i]], ";"))))
  dfCountriesPerYear$nrCountriesCumulated[i] <- length(unique(unlist(str_split(df$AU_CO[df$PY <= dfCountriesPerYear$year[i]], ";"))))
}
plot(dfCountriesPerYear[,1:2])
plot(dfCountriesPerYear[,c(1,3)])

countries2plot <- 15
countryByYearCumulated <- KeywordGrowth(df, Tag = "AU_CO", top = countries2plot)
countryByYear <- countryByYearCumulated
countryByYear[,2:(countries2plot+1)] <- apply(rbind(rep(0,countries2plot), countryByYear[,2:(countries2plot+1)]), 2, diff)
countryByYear <- pivot_longer(countryByYear, cols = 2:(countries2plot+1), names_to = "country", values_to = "publications")
countryByYear$lineType <- factor(rep(c(rep(1,ceiling(countries2plot/2)), rep(2,floor(countries2plot/2))),
                                     dim(countryByYear)[1]/countries2plot))

#colorVals <- rainbow(n = countries2plot, s = 1, v = 1, start = 0, end = 1-1/countries2plot)
#colorValsDark <- rainbow(n = countries2plot, s = 1, v = .50, start = 0, end = 1-1/countries2plot)
#colorVals[seq(1,length(colorVals),2)] <- colorValsDark[seq(1,length(colorVals),2)]
colorVals <- rep(rainbow(n = ceiling(countries2plot/2), s = 1, v = .90, start = 0, end = 1-1/ceiling(countries2plot/2)),2)
colorValsDark <- rep(rainbow(n = ceiling(countries2plot/2), s = 1, v = .50, start = 0, end = 1-1/ceiling(countries2plot/2)),2)
colorVals[seq(1,length(colorVals),2)] <- colorValsDark[seq(1,length(colorVals),2)]
colorVals <- colorVals[1:countries2plot]

#timecoursePlot <- ggplot(countryByYear, aes(x = Year, y = publications, color = country)) +
timecoursePlot <- ggplot(countryByYear[countryByYear$Year >= yearFrom & countryByYear$Year <= yearTo,], aes(x = Year, y = publications, color = country)) +
  theme_classic() +
  geom_line(aes(linetype = country)) +
  #geom_line() +
  scale_color_manual(values = colorVals, breaks = unique(countryByYear$country)) +
  scale_linetype_manual(values = c(rep("solid",ceiling(countries2plot/2)), rep("dashed",floor(countries2plot/2))), breaks = unique(countryByYear$country)); timecoursePlot
  #scale_color_brewer(type = "qual", palette = "Paired"); timecoursePlot

collabNetwork <- biblioNetwork(df, analysis = "collaboration", network = "countries", sep = ";", n = countries2plot)
collabPlot <- networkPlot(collabNetwork, type = "circle", size = TRUE,
                          remove.multiple = FALSE, edges.min = 10, Title = "")

collabNetworkAll <- biblioNetwork(df, analysis = "collaboration", network = "countries", sep = ";")
collabPlotAll <- networkPlot(collabNetworkAll, type = "circle", size = TRUE,
                             remove.multiple = FALSE, edges.min = 3, Title = "")

# save the plots 
set.seed(1)
pdf(paste0(parentFolder, "/plots/internationalCollabs_top15_min10.pdf"), width = 20/2.54, height = 20/2.54)
collabPlot <- networkPlot(collabNetwork, type = "circle", size = TRUE,
                          remove.multiple = FALSE, edges.min = 10, Title = "")
dev.off()

set.seed(1)
pdf(paste0(parentFolder, "/plots/internationalCollabs_all_min3.pdf"), width = 20/2.54, height = 20/2.54)
collabPlotAll <- networkPlot(collabNetworkAll, type = "circle", size = TRUE,
                             remove.multiple = FALSE, edges.min = 10, Title = "")
dev.off()


