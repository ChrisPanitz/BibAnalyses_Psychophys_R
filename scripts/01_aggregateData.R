library(bibliometrix)
library(here)

parentFolder <- here()

filenames <- list.files(paste0(parentFolder, "/rawData"), "bibData_Psychophysiology_*", full.names = TRUE)
M <- convert2df(filenames, dbsource = "wos", format = "plaintext")

saveRDS(M, file = paste0(parentFolder,"/rawData/allPapers.rds"))