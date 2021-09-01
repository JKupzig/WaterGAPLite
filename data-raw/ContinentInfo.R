## code to prepare `DATASET` dataset goes here

ContinentInfo <- read.csv(file.path(getwd(), "data-raw", "ContinentInfo.csv"), sep=";")
usethis::use_data(ContinentInfo, overwrite = TRUE, internal=T)
