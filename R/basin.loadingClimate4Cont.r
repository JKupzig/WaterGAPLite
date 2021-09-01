#' @title Loading climate from continental data
#' @description Function to read continental climate infromation
#' @param name name name of variable to be read
#' @param basIndex basinIndex to define which cells of continental raster belong to basin
#' @param transMatrix transformation Matrix to transform global data to basinwide data (not sure if this is correct)
#' @param DataDir filepath to data-folder - usually ...base/data
#' @param simPeriodDate DateVector defining the simulation period
#' @param cont continent as string (au, af, as, eu, na, sa)
#' @return Matrix with climate data (nrow=days, ncol=cells of basin)

basin.loadingClimate4Cont <- function(name="prec",basIndex, transMatrix,
                                       DataDir, simPeriodDate, cont){


  array_size <- length(basIndex)
  MatrixOut <- matrix(data=NA, nrow=length(simPeriodDate), ncol=array_size)

  root = file.path(DataDir, "climate", "continental", cont)

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

    #check if file exists, if not, return NA Matrix and throw warning
    filepath <- file.path(DataDir, "climate", "continental", cont, f)
    if (!file.exists(filepath)){
      warning(paste("File:", f, "does not exists in", file.path(DataDir, name, "continental", cont), "--> NA-Matrix returned or introduced when climateShortCut is used"))
      MatrixOut <- matrix(data=NA, nrow=length(simPeriodDate), ncol=array_size)
      break #breaks for loop to return NA
    } else {
      ClimateData <- readingUNF(file2read=f, DataDir=DataDir,
                                transMatrix=transMatrix, basIndex=basIndex,
                                name="climate", TypeClimate=2, cont=cont)

      filepath <- file.path(root, f)
      nLayerClimate <- getInfoFile(filepath)[[1]]
      Size <- getInfoFile(filepath)[[2]]
      Type <- getInfoFile(filepath)[[3]]

      indexYear <- which(as.numeric(format(simPeriodDate, "%Y")) == year)
      indexMonth <- which(as.numeric(format(simPeriodDate[indexYear], "%m")) == month)
      RowMatrix <- indexYear[indexMonth]
      MatrixOut[RowMatrix,] = ClimateData[1:length(RowMatrix),]
    }
  }
  rownames(MatrixOut) <- as.character(simPeriodDate)
  return(MatrixOut)
}
