# --- author: Christian Panitz
# --- encoding: UTF-8
# --- R version: 4.3.1 (2023-06-16) -- "Beagle Scouts"
# --- RStudio version: 2023.06.0
# --- script version: Oct 2024
# --- content: Run analyses & create plots on journal performace (publications, submissions, IF)


### header ###
fontSize <- 12
### end header ###


# load packages
library(here)
library(bibliometrix)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(psych)

# parent folder
parentFolder <- here()

# load  bibliometric data frame
filename <- paste0(parentFolder, "/rawData/allPapers.rds")
df <- readRDS(filename)



####################
### publications ###
####################

# create and format data frame for number of published papers by year 
dfPubs <- data.frame(table(df$PY))
names(dfPubs) <- c("year", "nrPubs")
dfPubs$year <- as.numeric(levels(dfPubs$year))

# add number of citations by year (not used for manuscript)
indCitPerYear <- data.frame(
  year = df$PY,
  citPerYear = df$TC / (2024 - df$PY)
)
citPerYear <- aggregate(citPerYear ~ year, data = indCitPerYear, FUN = mean)
dfPubs$citPerYear <- round(citPerYear$citPerYear,1)

# plot number of published papers
yMaxPub <- 280
pubPlot <- ggplot(data = dfPubs, aes(x = year, y = nrPubs)) +
                  theme_classic() +
                  geom_col(fill = "gray70") + 
                  geom_text(aes(y = nrPubs + yMaxPub/100, label = nrPubs), size = 1.5, fontface = "bold", color = "black") +
                  scale_x_continuous(name = "Year", limits = c(1963,2024), breaks = seq(5*ceiling(min(dfPubs$year)/5),max(dfPubs$year),5)) +
                  scale_y_continuous(limits = c(0,yMaxPub), name = "# of Published Articles", expand = c(0,0)) +
                  theme(axis.text = element_text(size = fontSize - 2, color = "black"),
                        axis.title = element_text(size = fontSize))
pubPlot



###################
### submissions ###
###################

# load  submission data
filename <- paste0(parentFolder, "/rawData/annualSubmissions_Psychophysiology_1980to2023.txt")
dfSub <- read.csv(filename, sep = ";")

# some descriptive statistics on submission numbers
describe(dfSub$submissions)
quantile(dfSub$submissions)

# plot submission numbers
yMaxSub <- 800
subPlot <- ggplot(data = dfSub, aes(x = year, y = submissions)) +
  theme_classic() +
  geom_col(fill = "gray70") + 
  geom_text(aes(y = submissions + yMaxSub/100, label = submissions), size = 1.5, fontface = "bold", color = "black") +
  scale_x_continuous(name = "Year", limits = c(1963, 2024), breaks = seq(5*ceiling(min(dfSub$year)/5),max(dfSub$year),5)) +
  scale_y_continuous(limits = c(0,yMaxSub), name = "# of Submitted Articles", expand = c(0,0)) +
  theme(axis.text = element_text(size = fontSize - 2, color = "black"),
        axis.title = element_text(size = fontSize))
subPlot



#####################
### impact factor ###
#####################

# load  impact factor data
filename <- paste0(parentFolder, "/rawData/impactFactor_Psychophysiology_1997to2021.txt")
dfIF <- read.csv(filename, sep = ";")
dfIF$IF <- round(dfIF$IF, 2)

# show impact factor across years and make quick plot
dfIF
describe(dfIF$IF)
quantile(dfIF$IF)

# plot impact factor over years
yMaxIF <- 5.0
ifPlot <- ggplot(data = dfIF, aes(x = year, y = IF)) +
                 theme_classic() +
                 geom_col(fill = "gray70") +
                 geom_text(aes(y = IF + yMaxIF/100, label = IF), size = 1.5, fontface = "bold", color = "black") +
                 scale_x_continuous(name = "Year", limits = c(1963, 2024), breaks = seq(5*ceiling(min(dfIF$year)/5),max(dfIF$year),5)) +
                 scale_y_continuous(limits = c(0,yMaxIF), name = "2-Year Impact Factor", expand = c(0,0)) +
                 theme(axis.text = element_text(size = fontSize - 2, color = "black"),
                       axis.title = element_text(size = fontSize))
ifPlot



#####################
### combine plots ###
#####################

# combine plots
combPlot <- ggarrange(pubPlot, subPlot, ifPlot,
                      nrow = 3, ncol = 1,
                      labels = c("A","B","C"),
                      align = "v")
combPlot

# save it
ggsave(paste0(parentFolder, "/plots/nrPub_nrSub_IF.pdf"), combPlot,
       width = 20, height = 30, units = "cm")

ggsave(paste0(parentFolder, "/plots/nrPub_nrSub_IF.png"), combPlot,
       width = 20, height = 30, units = "cm", dpi = 600)