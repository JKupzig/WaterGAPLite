#' @title Loading climate data for basin
#' @description Function to load climate when initiliazing climate object (calls other loadingClimate-functions, depending on climateFormat)
#' @param cont continent as string (au, af, as, eu, na, sa)
#' @param grdc_number  grdc_number of basin (is used as ID in this case, however, later on it is also used to load Discharge data)
#' @param basin_index basinIndex information to determine which cells belong to the basin
#' @param trans_matrix transformation Matrix to transform global data to basinwide data (not sure if this is correct)
#' @param sim_period_date DateVector defining the simulation period
#' @param climate_shortcut filepath to climateshortcut-folder - usually ...base/data/ClimateShortcut
#' @param data_dir filepath to data-folder - usually ...base/data
#' @param wg2with5min_mask transformation information to get basinwide information from global met. data
#' @param watch_mask  transformation information to get basinwide information from global met. data
#' @param type_name of variable to be read
#' @param read_newly information if climate data is forced to be read newly (T) or not (F, ClimateShortCut will be used)
#' @param climate_format defining format of climate (global, continental or basin)
#' @return Matrix with climate data (nrow=days, ncol=cells of basin)
basin.loadingClimate <- function(cont, grdc_number, basin_index, trans_matrix,
                               sim_period_date, climate_shortcut, data_dir,
                                wg2with5min_mask, watch_mask,
                                type_name = "prec",
                                read_newly = FALSE, climate_format) {

  # first: check if there exists already prepared timeseries 
  # or if function is forced to prepare all timeseries new
  fileName <- file.path(climate_shortcut, 
                  paste0(as.character(grdc_number),"_", type_name, ".rds"))

  index2read <- 0
  if (file.exists(fileName) && read_newly == FALSE) {
    tmp <- readRDS(fileName)
    timeperiod <- (as.Date(tmp[,1], origin ="1970-01-01"))
    index2read <- sim_period_date %in% timeperiod
  }

  if ((sum(index2read) == 0) || (read_newly == TRUE) || (!file.exists(fileName)))  {
    #timeseries needs to be read in as whole

    if (climate_format == "global"){
      var <- basin.loading_climate_ewembi(name = type_name, basin_index,
                                          trans_matrix, data_dir,
                                          wg2with5min_mask, watch_mask,
                                          sim_period_date
                                          )
    
    } else if (climate_format == "continental") {
      var <- basin.loading_climate_continental(name = type_name, basin_index,
                                       trans_matrix, data_dir,
                                       sim_period_date, cont
                                       )

    } else if (climate_format == "basin") {
      var <- basin.loading_climate_basin(name = type_name, grdc_number,
                                        basin_index, data_dir,
                                        sim_period_date
                                        )
    } else {
      stop("ClimateFormat not specified properly - use 'global' or 
            'continental' or 'basin', when specified global: 
            data in data/climate/global is used, when specified 
            continental: data in data/climate/continental/[cont] is used, 
            when specified basin: data in data/climate/basin/id_
            climatetype.rds is used"
            )
    }
    
    date2write <- as.Date(rownames(var))
    var2write <- cbind(date2write, var)
    rownames(var2write) <- NULL
    colnames(var2write) <- NULL
    
    ###### Special cases ##################################
    # data is read in in to 31.12.1979 and exists already from 1980.01.01 on
    if (file.exists(fileName)){
      if (min(timeperiod) == max(date2write) + 1) {
        var2write <- rbind(var2write, tmp)
      # data is read in in from 01.01.1980 and exists already til 31.12.1979
      } else if (max(timeperiod) == min(date2write) - 1) {
        var2write <- rbind(tmp, var2write)
      }
    }
############################################################
    saveRDS(var2write, file = fileName)

  # checking if timeseries is already completely written out
  } else if (sum(index2read) == length(index2read)) {

    var <- readRDS(fileName)
    timeperiod <- (as.Date(var[, 1], origin = "1970-01-01"))
    var <- var[, 2:ncol(var)]
    rownames(var) <- as.character(timeperiod)

    index2use <- timeperiod %in% sim_period_date
    var <- var[index2use, ]

  # at least some overlapping exists
  } else {
    warning("Please make sure that data in climateShortcut 
            correspondent to used climateFormat. If not sure, 
            better use force2read=T to ensure that meteorological 
            data has same origin (data is read entirely new with this option).")
    
    #checking if timeseries needs to be extended in the beginning:
    var_start <- NA
    sim_period_date_1 <- NA
    if (index2read[1] == FALSE) {
      #getting simperiod that needs to be read in
      id <- which(index2read[2:length(index2read)] != data.table::shift(index2read,1)[2:length(index2read)])[1]
      sim_period_date_1 <- sim_period_date[1:id]
      
      #reading data that is missing form climate file
      if (climate_format == "global"){
        var_start <- basin.loading_climate_ewembi(name = type_name, basin_index,
                                                  trans_matrix, data_dir, 
                                                  wg2with5min_mask, 
                                                  watch_mask, sim_period_date_1)

      } else if (climate_format == "continental") {
        var_start <- basin.loading_climate_continental(name = type_name, 
                                                      basin_index,
                                                      trans_matrix, data_dir,
                                                      sim_period_date_1, cont)
      
      } else if (climate_format == "basin") {
        var_start <- basin.loading_climate_basin(name="prec", grdc_number,
                                                basin_index, data_dir,
                                                sim_period_date_1)
      } else {
        stop("ClimateFormat not specified properly - 
              use 'global' or 'continental' or 'basin' ,
              when specified global: data in data/climate/global is used, 
              when specified continental: data in 
              data/climate/continental/[cont] is used, 
              when specified basin: data in 
              data/climate/basin/id_climatetype.rds is used"
              )
      }

    }

    #checking if timeseries needs to be extended in the end:
    var_end <- NA
    sim_period_date_2 <- NA
    if (index2read[length(index2read)] == FALSE) {
      #getting simperiod that needs to be read in
      id <- which(index2read[2:length(index2read)] != data.table::shift(index2read,1)[2:length(index2read)])
      id <- id[length(id)]
      sim_period_date_2 <- sim_period_date[(id + 1):length(sim_period_date)]
      
      #reading data that is missing form climate file
      if (climate_format == "global") {
        var_end <- basin.loading_climate_ewembi(name = type_name, basin_index,
                                                trans_matrix,
                                                data_dir, wg2with5min_mask,
                                                watch_mask,
                                                sim_period_date_2)
      
      } else if (climate_format == "continental") {
        var_end <- basin.loading_climate_continental(name = type_name,
                                                     basin_index,
                                                     trans_matrix,
                                                     data_dir,
                                                     sim_period_date_2,
                                                     cont)
      
      } else if (climate_format=="basin") {
        var_end <- basin.loading_climate_basin(name = "prec", grdc_number,
                                                basin_index,
                                                data_dir, sim_period_date_2)
      
      } else {
        stop("ClimateFormat not specified properly - 
              use 'global' or 'continental' or 'basin' ,
              when specified global: data in data/climate/global is used, 
              when specified continental: data in 
              data/climate/continental/[cont] is used, 
              when specified basin: data in 
              data/climate/basin/id_climatetype.rds is used"
              )
      }
    }

    #read data completly from textfile
    var <- readRDS(fileName)
    timeperiod_r <- (as.Date(var[, 1], origin = "1970-01-01"))
    var <- var[, 2:ncol(var)]
    rownames(var) <- as.character(timeperiod_r)

    #combine everything
    var <- rbind(var_start, var)
    var <- rbind(var, var_end)
    var <- var[rowSums(is.na(var)) != ncol(var), ]

    #writing down the extended version
    date2write <- as.Date(rownames(var))
    var2write <- cbind(date2write, var)
    rownames(var2write)=NULL; colnames(var2write) <- NULL
    saveRDS(var2write, file = fileName)

    #selecting only period that is needed for simulation
    extended_timeperiod <-  c(sim_period_date_1, timeperiod, sim_period_date_2)
    extended_timeperiod <- extended_timeperiod[!is.na(extended_timeperiod)]
    index2use <- extended_timeperiod %in% sim_period_date
    var <- var[index2use, ]

  }

  rownames(var) <- NULL
  #check in the end if there are NA values included in timesieres
  if (sum(is.na(var)) > 0) {
    warning("NA values included in timeseries -
    please check meteorological input!")
  }

  return(var)
}
