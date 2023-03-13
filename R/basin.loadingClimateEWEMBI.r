#' @title Loading worldwide EWEMBI data 
#' @description Function to read worldwide (EWEMBI) climate data for basin
#' @param name name of variable to be read
#' @param basIndex basinIndex to define which cells of continental raster belong to basin
#' @param transMatrix transformation Matrix to transform global data to basinwide data (not sure if this is correct)
#' @param DataDir filepath to data-folder - usually ...base/data
#' @param G_WG3_WG2WITH5MIN transformation information to get basinwide information from global met. data
#' @param G_WG3_WATCH transformation information to get basinwide information from global met. data
#' @param simPeriodDate DateVector defining the simulation period
#' @return Matrix with climate data (nrow=days, ncol=cells of basin)


basin.loadingClimateEWEMBI <- function(name="prec",basIndex, transMatrix,
                                       DataDir, G_WG3_WG2WITH5MIN, G_WG3_WATCH,
                                       simPeriodDate){

  array_size <- length(basIndex)
  #only climatedata for basin is read to save memory
  MatrixOut <- matrix(data=NA, nrow=length(simPeriodDate), ncol=array_size)

  root = file.path(DataDir, "climate", "global")

  startYear <- as.numeric(format(min(simPeriodDate), "%Y"))
  endYear <- as.numeric(format(max(simPeriodDate), "%Y"))

  startMonth <- as.numeric(format(min(simPeriodDate), "%m"))
  endMonth <- as.numeric(format(max(simPeriodDate), "%m"))

  startDay <- as.numeric(format(min(simPeriodDate), "%d")) #not used at the moment
  endDay <- as.numeric(format(max(simPeriodDate), "%d")) #not used at the moment

  #special condition, when only one year needs to be read in
  if (startYear == endYear){
    Months <- seq(1,12,1)
    Years <- rep(startYear, 12)
  } else if (startYear== (endYear-1)) {

    uniqueYears <- seq(startYear, endYear, 1)
    FirstMonths <- seq(startMonth,12,1)
    LastMonths <- seq(1,endMonth,1)

    Months <- c(FirstMonths,rep(1:12,length(uniqueYears)-2), LastMonths)

    Years = c(rep(uniqueYears[1], length(FirstMonths)),
              rep(uniqueYears[length(uniqueYears)], length(LastMonths)))

  } else {
    uniqueYears <- seq(startYear, endYear, 1)
    FirstMonths <- seq(startMonth,12,1)
    LastMonths <- seq(1,endMonth,1)

    Months <- c(FirstMonths,rep(1:12,length(uniqueYears)-2), LastMonths)

    Years = c(rep(uniqueYears[1], length(FirstMonths)),
              sort(rep(uniqueYears[2]:uniqueYears[length(uniqueYears)-1], 12)),
              rep(uniqueYears[length(uniqueYears)], length(LastMonths)))
  }

  file2read <- sprintf("GPREC_%d_%d.31.UNF0", Years[1], Months[1])
  StorageSize <- file.info(file.path(root, file2read))$size/(4*31)

  maskInfo <- G_WG3_WG2WITH5MIN #by default
  if (StorageSize == 67420) { maskInfo <- G_WG3_WATCH } #DATEIEN IN FOLDER SOLLTEN ALLE GLEICHE GRÖßE HABEN!

  if (name == "prec"){
    Stringname = "GPREC_%d_%d.31.UNF0"
  } else if (name == "shortwave"){
    Stringname = "GSHORTWAVE_%d_%d.31.UNF0"
  } else if (name == "longwave"){
    Stringname = "GLONGWAVE_DOWN_%d_%d.31.UNF0"
  } else if (name == "temp"){
    Stringname = "GTEMP_%d_%d.31.UNF0"
  }

  for (i in 1:length(Months)) {

    month = Months[i]
    year = Years[i]
    f = sprintf(Stringname, year, month)

    ClimateData <- readingUNF(f, DataDir, transMatrix=NULL, basIndex=NULL, name="climate", TypeClimate=1)

    filepath <- file.path(root, f)
    nLayerClimate <- getInfoFile(filepath)[[1]]
    Size <- getInfoFile(filepath)[[2]]
    Type <- getInfoFile(filepath)[[3]]


    indexYear <- which(as.numeric(format(simPeriodDate, "%Y")) == year)
    indexMonth <- which(as.numeric(format(simPeriodDate[indexYear], "%m")) == month)
    RowMatrix <- indexYear[indexMonth]

    ###separating layers from climate data
    for (j in 1:length(RowMatrix)){
      ClimateData_day <- sapply(1:(length(ClimateData)/nLayerClimate),
                                function(x) ClimateData[(nLayerClimate*(x-1))+j])
      MatrixOut[RowMatrix[j],] = ClimateData_day[maskInfo]
    }
  }

  rownames(MatrixOut) <- as.character(simPeriodDate)

  return(MatrixOut)
}
