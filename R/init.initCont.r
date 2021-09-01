#' @title Initializing continentalObject
#' @description  Function to initialize the continent where basin is located
#' @param cont continent as string where basin is part of (as, af, au, na, eu, sa)
#' @param path2base filepath to folder where folder for data, source_code and output are located
#' @return new continent object for continent where basin is located
#' @importFrom utils read.csv
init.initCont <- function(cont, path2base){ # options: sa, eu, af, au, na, sa ....
  
  #readin infoFile (could be part of package data!):
  path2use <- init.gettingPaths(cont, base=path2base)
  info <- read.csv(file.path(path2use[["DataDir"]], "ContinentInfo.csv"), sep=";")
  #print(info)
  
  newCont <- new("Continent", 
                 contName = as.character(cont),
                 ng = as.numeric(info[cont][[1]][1]) ,
                 ng_land = as.numeric(info[cont][[1]][2]),
                 nrow=as.integer(info[cont][[1]][3]),
                 ncol=as.integer(info[cont][[1]][4]),
                 xllCorner=as.numeric(info[cont][[1]][7]),
                 yllCorner=as.numeric(info[cont][[1]][8]),
                 cellsize=as.numeric(info[cont][[1]][9]),
                 ng_gcrc=as.integer(info[cont][[1]][3])*as.integer(info[cont][[1]][4]),
                 corRow=as.integer(info[cont][[1]][11]),
                 corCol=as.integer(info[cont][[1]][12]),
                 
                 DataDir=path2use[["DataDir"]],
                 BaseDir=path2use[["BaseDir"]],
                 OutputDir=path2use[["OutputDir"]],
                 climateShortcut=path2use[["climateShortcut"]])
  
  
  return(newCont)
  
}
