#' @title Loading climate data for basin
#' @description Function to load climate when initiliazing climate object (calls other loadingClimate-functions, depending on climateFormat)
#' @param cont continent as string (au, af, as, eu, na, sa)
#' @param grdc_number  grdc_number of basin (is used as ID in this case, however, later on it is also used to load Discharge data)
#' @param basinIndex basinIndex information to determine which cells belong to the basin
#' @param transMatrix transformation Matrix to transform global data to basinwide data (not sure if this is correct)
#' @param simPeriodDate DateVector defining the simulation period
#' @param climateShortcut filepath to climateshortcut-folder - usually ...base/data/ClimateShortcut
#' @param DataDir filepath to data-folder - usually ...base/data
#' @param G_WG3_WG2WITH5MIN transformation information to get basinwide information from global met. data
#' @param G_WG3_WATCH  transformation information to get basinwide information from global met. data
#' @param typeName of variable to be read
#' @param readNewly information if climate data is forced to be read newly (T) or not (F, ClimateShortCut will be used)
#' @param ClimateFormat defining format of climate (global, continental or basin)
#' @return Matrix with climate data (nrow=days, ncol=cells of basin)
 

basin.loadingClimate <- function(cont, grdc_number, basinIndex, transMatrix, simPeriodDate, climateShortcut,
                                 DataDir, G_WG3_WG2WITH5MIN, G_WG3_WATCH, typeName="prec", readNewly=F, ClimateFormat){

  #first: check if there exists already prepared timeseries or if function is forced to prepare all timeseries new
  fileName <- file.path(climateShortcut, paste0(as.character(grdc_number),"_", typeName, ".rds"))
  array_size <- length(basinIndex)

  index2read <- 0
  if (file.exists(fileName) & readNewly==F) {
    tmp <- readRDS(fileName)
    timeperiod <- (as.Date(tmp[,1], origin ="1970-01-01")) #first column are dates
    index2read <- simPeriodDate %in% timeperiod
  }

  if ( (sum(index2read)==0) | (readNewly==T) | (!file.exists(fileName)) )  {  #timeseries needs to be read in as whole

    if (ClimateFormat=="global"){
      var <- basin.loadingClimateEWEMBI(name=typeName, basinIndex, transMatrix,
                                            DataDir, G_WG3_WG2WITH5MIN, G_WG3_WATCH,
                                            simPeriodDate)
    } else if (ClimateFormat=="continental") {
      var <- basin.loadingClimate4Cont(name=typeName, basinIndex, transMatrix,
                                        DataDir, simPeriodDate, cont)
    } else if (ClimateFormat=="basin") {
      var <- basin.loadingClimate4Basin(name=typeName, grdc_number, basinIndex,
                                             DataDir, simPeriodDate)
    } else {
      stop("ClimateFormat not specified properly - use 'global' or 'continental' or 'basin' ,
           when specified global: data in data/climate/global is used, 
           when specified continental: data in data/climate/continental/[cont] is used, 
           when specified basin: data in data/climate/basin/id_climatetype.rds is used")
    }
    
    Date2write <- as.Date(rownames(var))
    var2write <- cbind(Date2write, var)
    rownames(var2write)=NULL; colnames(var2write) <- NULL
    
    ###### Special cases #####################################################################################
    # data is read in in to 31.12.1979 and exists already from 1980.01.01 on
    if (file.exists(fileName)){
      if (min(timeperiod) == max(Date2write)+1){
        var2write <- rbind(var2write, tmp)
      # data is read in in from 01.01.1980 and exists already til 31.12.1979   
      } else if (max(timeperiod) == min(Date2write)-1) {
        var2write <- rbind(tmp, var2write) 
      } 
    }
    ##########################################################################################################
    
    saveRDS(var2write, file = fileName)
    
  } else if (sum(index2read)==length(index2read)) {  #checking if timeseries is already completely written out

    var <- readRDS(fileName)
    timeperiod <- (as.Date(var[,1], origin ="1970-01-01")) #first column are dates
    var <- var[,2:ncol(var)]
    rownames(var) <- as.character(timeperiod)
    
    index2use <- timeperiod %in% simPeriodDate
    var <- var[index2use,]

  } else { #at least some overlapping exists
    warning("Please make sure that data in climateShortcut correspondent to used climateFormat. If not sure, better use 
            force2read=T to ensure that meteorological data has same origin (data is read entirely new with this option).")
    #checking if timeseries needs to be extended in the beginning:
    var_start <- NA
    simPeriodDate_1 <- NA
    if (index2read[1]==F){
      #getting simperiod that needs to be read in
      id <- which(index2read[2:length(index2read)] != data.table::shift(index2read,1)[2:length(index2read)])[1]
      simPeriodDate_1 <- simPeriodDate[1:id]
      
      #reading data that is missing form climate file
      if (ClimateFormat=="global"){
        var_start <- basin.loadingClimateEWEMBI(name=typeName, basinIndex, transMatrix,
                                          DataDir, G_WG3_WG2WITH5MIN, G_WG3_WATCH,
                                          simPeriodDate_1)
      } else if (ClimateFormat=="continental") {
        var_start <- basin.loadingClimate4Cont(name=typeName, basinIndex, transMatrix,
                                         DataDir, simPeriodDate_1, cont)
      } else if (ClimateFormat=="basin") {
        var_start <- basin.loadingClimate4Basin(name="prec", grdc_number, basinIndex,
                                               DataDir, simPeriodDate_1)
      } else {
        stop("ClimateFormat not specified properly - use 'global' or 'continental' or 'basin' ,
           when specified global: data in data/climate/global is used, 
           when specified continental: data in data/climate/continental/[cont] is used, 
           when specified basin: data in data/climate/basin/id_climatetype.rds is used")
      }

    }

    #checking if timeseries needs to be extended in the end:
    var_end <- NA
    simPeriodDate_2 <- NA
    if (index2read[length(index2read)]==F){
      #getting simperiod that needs to be read in
      id <- which(index2read[2:length(index2read)] != data.table::shift(index2read,1)[2:length(index2read)])
      id <- id[length(id)]
      simPeriodDate_2 <- simPeriodDate[(id+1):length(simPeriodDate)]
      
      #reading data that is missing form climate file
      if (ClimateFormat=="global"){
        var_end <- basin.loadingClimateEWEMBI(name=typeName, basinIndex, transMatrix,
                                                DataDir, G_WG3_WG2WITH5MIN, G_WG3_WATCH,
                                                simPeriodDate_2)
      } else if (ClimateFormat=="continental") {
        var_end <- basin.loadingClimate4Cont(name=typeName, basinIndex, transMatrix,
                                               DataDir, simPeriodDate_2, cont)
      } else if (ClimateFormat=="basin") {
        var_end <- basin.loadingClimate4Basin(name="prec", grdc_number, basinIndex,
                                                DataDir, simPeriodDate_2)
      } else {
        stop("ClimateFormat not specified properly - use 'global' or 'continental' or 'basin' ,
           when specified global: data in data/climate/global is used, 
           when specified continental: data in data/climate/continental/[cont] is used, 
           when specified basin: data in data/climate/basin/id_climatetype.rds is used")
      }
    }

    #read data completly from textfile
    var <- readRDS(fileName)
    timeperiod_r <- (as.Date(var[,1], origin ="1970-01-01")) #first column are dates
    var <- var[,2:ncol(var)]
    rownames(var) <- as.character(timeperiod_r)
    
    #combine everything
    var <- rbind(var_start, var)
    var <- rbind(var, var_end)
    var <- var[rowSums(is.na(var)) != ncol(var), ]

    #writing down the extended version
    Date2write <- as.Date(rownames(var))
    var2write <- cbind(Date2write, var)
    rownames(var2write)=NULL; colnames(var2write) <- NULL
    saveRDS(var2write, file = fileName)
    

    #selecting only period that is needed for simulation
    extentedTimePeriod <-  c(simPeriodDate_1, timeperiod, simPeriodDate_2)
    extentedTimePeriod <- extentedTimePeriod[!is.na(extentedTimePeriod)]
    index2use <- extentedTimePeriod %in% simPeriodDate
    var <- var[index2use,]

  }

  rownames(var) <- NULL
  #check in the end if there are NA values included in timesieres
  if (sum(is.na(var))>0) { warning("NA values included in timeseries - please check meteorological input!")}

  return(var)
}
