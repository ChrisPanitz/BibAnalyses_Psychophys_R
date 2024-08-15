### header ###
minOccur <- 5 # minimum of occurrences of multi-word combinations to be classified as key term
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




# check for common 2-/3-/4-word combinations
# termExtraction only shows top results, however, tableTag does not include the option to spare number
# tableTag is used to extract multi-word key terms for double-checking but termExtraction can be used to manually screen for important numeric word combinations
termExtraction(df, Field = "TI", ngrams = 4, remove.numbers = FALSE, verbose = TRUE)
tabTags4 <- tableTag(df, Tag = "TI", ngrams = 4)
termExtraction(df, Field = "TI", ngrams = 3, remove.numbers = FALSE, verbose = TRUE)
tabTags3 <- tableTag(df, Tag = "TI", ngrams = 3)
termExtraction(df, Field = "TI", ngrams = 2, remove.numbers = FALSE, verbose = TRUE)
tabTags2 <- tableTag(df, Tag = "TI", ngrams = 2)
tableTag(df, Tag = "TI", ngrams = 1)

# write txt file with all 4-to-2 word combinations with at least [minOccur] occurrences
write.table(c(dimnames(tabTags4[tabTags4 >= minOccur])[["Tab"]],
              dimnames(tabTags3[tabTags3 >= minOccur])[["Tab"]],
              dimnames(tabTags2[tabTags2 >= minOccur])[["Tab"]]),
            "terms/compoundTerms_TI_raw.txt", sep = ";", row.names = FALSE, col.names = FALSE)


df <- termExtraction(df, Field = "TI", ngrams = 1, remove.numbers = FALSE, verbose = TRUE)

terms2keep_TI <- read.table("terms/compoundTerms_TI_selected.txt", sep = ";", col.names = FALSE)
  
replace2keep_TI <- gsub(" ",";",terms2keep_TI)
replaceWith_TI <- gsub(" ","-",terms2keep_TI)

for (i in 1:length(terms2keep_TI)){
  df$TI_TM <- gsub(replace2keep_TI[i],replaceWith_TI[i],df$TI_TM)
}


test <- tableTag(df, Tag = "TI_TM")

terms2delete <- c("potential","potentials","effects","response","responses","study","processing","activity","task","evidence","stimuli","effect","stimulus","analysis","differences", "stimuli")
synonyms <- c("erp; erps; event-related potentials; event-related potential",
              "heart rate; heart-rate")
 nwTitle2000 <- biblioNetwork(df[is.element(df$PY, 2000:2009),], analysis = "co-occurrences", network = "titles", 
                          short = TRUE, remove.terms = terms2delete, synonyms = synonyms)
nwTitle <- biblioNetwork(df, analysis = "co-occurrences", network = "titles", 
                             short = TRUE, remove.terms = terms2delete, synonyms = synonyms)
set.seed(1)
nwPlot <- networkPlot(nwTitle, n = 50, type = "auto", edges.min = 10, remove.isolates = TRUE); nwPlot




summary(df$TI_TM)
conceptualStructure(df, field = "TI")




tableTag(df, Tag = "TI_TM", ngrams = 1)

?KeywordGrowth(df, Tag = "TI_TM")


#type="auto"		Automatic layout selection
#type="circle"		Circle layout
#type="sphere"		Sphere layout
#type="mds"		Multidimensional Scaling layout
#type="fruchterman"		Fruchterman-Reingold layout
#type="kamada"		Kamada-Kawai layout


# Example 1: Term extraction from titles

data(scientometrics, package = "bibliometrixData")

# vector of compound words
keep.terms <- c("cocitation analysis","co-citation analysis","bibliographic coupling")

# term extraction
scientometrics <- termExtraction(scientometrics, Field = "TI", ngrams = 1,
                                 remove.numbers=TRUE, remove.terms=NULL, keep.terms=keep.terms, verbose=TRUE)







# 
# "event-related brain potential study",
# "resting heart rate variability",
# "evoked heart rate response",
# "rapid serial visual presentation",
# "smooth pursuit eye movements",
# # 4-word terms with < 5 occurences
# "",
# "","",""
# "heart rate variability",
# "event-related brain potentials",
# "respiratory sinus arrhythmia",
# "event-related brain potential",
# "concealed information test",
# "acute psychological stress",
# "late positive potential",
# "autonomic nervous system",
# "lateralized readiness potential",
# "galvanic skin response",
# "auditory evoked potentials",
# "auditory selective attention",
# "electrodermal orienting reponse",
# "major depressive disorder",
# "error-related brain activity",
# "frontal eeg asymmetry",
# "skin conductance response",
# "skin conductance responses",
# "attention-deficit hyperactivity disorder",
# "contingent negative variation",
# "event-related potential correlates",
# "heart period variability",
# "posttraumatic stress disorder",
# "heart rate",
# "event-related potentials",
# "event-related potential",
# "brain potentials",
# "cardiovascular reactivity",
# "orienting response",
# "selective attention",
# "mismatch negativity",
# "electrodermal activity",
# "brain activity",
# "reaction time",
# "psychological stress",
# "blood pressure",
# "individual differences",
# "cognitive control",
# "spatial attention",
# "startle reflex")
