# --- author: Christian Panitz
# --- encoding: UTF-8
# --- R version: 4.3.1 (2023-06-16) -- "Beagle Scouts"
# --- RStudio version: 2023.06.0
# --- script version: Oct 2024
# --- content: Read txt files of WoS output, create bibliometrix data frame and save it

# load packages
library(bibliometrix)
library(here)

# set parent folder of project
parentFolder <- here()

# read txt files and creat data frame
filenames <- list.files(paste0(parentFolder, "/rawData"), "bibData_Psychophysiology_*", full.names = TRUE)
M <- convert2df(filenames, dbsource = "wos", format = "plaintext")

#save data frame
saveRDS(M, file = paste0(parentFolder,"/rawData/allPapers.rds"))