### header ###
showEntries <- 25 # how many objects (i.e., top papers, authors, keywords) to show in the summary results
### end header ###


# load packages
library(here)
library(bibliometrix)
library(dplyr)

# parent folder
parentFolder <- here()

# load  bibliometric data frame
filename <- paste0(parentFolder, "/rawData/allPapers.rds")
df <- readRDS(filename)

# Extract country information of author affiliations
df <- metaTagExtraction(df, Field = "AU_CO")

### missing data in percent ###
# quick check with bibliometrix
missingData(df)

# now manually for different years
# across the entire data set
missVal <- round(apply(is.na(df),2,FUN = sum) / dim(df)[1] * 100, 1)

# by year
year <- sort(unique(df$PY))
missValByYear <- missVal
for (i in 1:length(year)){
  missValByYear <- rbind(missValByYear,
                         round(apply(is.na(df[df$PY == year[i],]),2,FUN = sum) /
                         dim(df[df$PY == year[i],])[1] * 100, 1))
}
missValByYear <- missValByYear[-1,]
missValByYear <- cbind(year, missValByYear)

# show missing data by year for author, title, abstract, author keyword (DE), and country data
missValByYear[,c("year", "AU", "TI", "AB", "DE", "AU_CO")]

# run standard summary analyses
results <- biblioAnalysis(df)
#results <- biblioAnalysis(df[df$PY > 2014,])

# ...and show them
options(width = 100)
S <- summary(object = results, k = showEntries, pause = FALSE)

# summary plots
plot(x = results, k = showEntries, pause = FALSE)