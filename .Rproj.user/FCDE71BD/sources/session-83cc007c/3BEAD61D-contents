# header
fontSize <- 12

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


# load  submission data
filename <- paste0(parentFolder, "/rawData/annualSubmissions_Psychophysiology_1980to2023.txt")
dfSub <- read.csv(filename, sep = ";")
dfSub
describe(dfSub$submissions)
quantile(dfSub$submissions)

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



# load  impact factor data
filename <- paste0(parentFolder, "/rawData/impactFactor_Psychophysiology_1997to2021.txt")
dfIF <- read.csv(filename, sep = ";")
dfIF$IF <- round(dfIF$IF, 2)

# show impact factor across years and plot it
dfIF
describe(dfIF$IF)
quantile(dfIF$IF)



# ###
# dfPubs$IF <- NA
# dfPubs$IF[dfPubs$year >= 1997 & dfPubs$year <= 2021] <- dfIF$IF
# 
# citPlot <- ggplot(data = dfPubs, aes(x = year)) +
#   theme_classic() +
#   geom_col(aes(y = citPerYear), fill = "gray50") + 
#   geom_text(aes(y = 0.1, label = citPerYear), size = 2, fontface = "bold", color = "white") +
#   geom_line(aes(y = IF), color = "darkred") + 
#   geom_point(aes(y = IF), color = "darkred") + 
#   geom_text(aes(y = IF-0.3, label = IF), size = 2, fontface = "bold", color = "darkred") +
#   scale_x_continuous(name = "Year", breaks = seq(min(dfPubs$year),max(dfPubs$year),2)) +
#   scale_y_continuous(limits = c(0,7), name = "mean # of citations per year since publication",
#                      sec.axis = dup_axis(name = "2-year impact factor")) +
#   theme(axis.line.y.right = element_line(color = "darkred"),
#         axis.ticks.y.right = element_line(color = "darkred"),
#         axis.text.y.right = element_text(color = "darkred"),
#         axis.title.y.right = element_text(color = "darkred"))
# citPlot


###
# by year
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
