#' @title Loading basinwide climate data
#' @description Function to read basinwide climate data
#' @param name name of variable to be read
#' @param grdc_number grdc number of basin that is used to save climate data in ClimateShortcut-folder
#' @param basIndex basinIndex to define which cells of continental raster belong to basin
#' @param DataDir filepath to data-folder - usually ...base/data
#' @param simPeriodDate DateVector defining the simulation period
#' @return Matrix with climate data (nrow=days, ncol=cells of basin)
#' @importFrom utils read.table
#' 
basin.loadingClimate4Basin <- function(name="prec", grdc_number, basIndex,
                                      DataDir, simPeriodDate){
  
  array_size <- length(basIndex)
  root <- file.path(DataDir, "climate", "basin", paste0(as.character(grdc_number),"_", name, ".txt"))
  
  if (!file.exists(root)){
    warning(paste("File:",  paste0(as.character(grdc_number),"_", name, ".txt") , "does not exists in", file.path(DataDir, name, "basin"), "--> NA-Matrix returned or introduced when climateShortCut is used"))
    tmp <- matrix(data=NA, nrow=length(simPeriodDate), ncol=array_size)
    rownames(tmp) <- as.character(simPeriodDate)
  
  } else {
    tmp <-data.table::fread(root, skip=2)
    tmp <- unlist(tmp, use.names = F)
    tmp <- matrix(tmp, nrow=length(tmp)/array_size, ncol=array_size)
    
    startDate <-read.table(file =root,header = F,nrows = 1, stringsAsFactors=F)
    ts <- seq(as.Date(startDate$V1, format="%Y-%m-%d"), as.Date(startDate$V1, format="%Y-%m-%d")+nrow(tmp)-1, by="day")
    
    id2use <- which(ts %in% simPeriodDate)
    tmp <- tmp[id2use,] 
    
    rownames(tmp) <- as.character(ts[id2use])
  }
  
  return(tmp)
}
