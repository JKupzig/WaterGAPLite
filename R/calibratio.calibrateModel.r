#' @title  Function to calibrate basin
#' @description gamma is changed and gamma value is searched for which mean deviation between simulated and observed discharge is minimal
#' @param basin_object BasinObject to be calibrated (hast to be in global environment)
#' @param basin_list Model Input for Basin
#' @param settings Vector for settings to define which model setting should be used to run the model
#' @param nwarm_up number of years that should be used as warm-up, i.e. that should not be considered in model run
#' @param start_val gamma value to start calibration, e.g. c(2.5)
#' @param upper_bound upper value for gamma, e.g. c(5.0)
#' @param lower_bound lower value gamma, e.g. c(0.1)
#' @return information about calibration result
#' @export
calibration.calibrate_model <- function(basin_object, basin_list,
                                        settings,
                                        nwarm_up = 5,
                                        start_val = c(2.5),
                                        upper_bound = c(0.1),
                                        lower_bound = c(5.0)) {
          
  message("CALIBRATION INFO:
  At the moment, only gamma can be varied, 
  function to be optimized is daily absolute 
  mean deviation between simulated and observed discharge
  search algorithm is bobyqa from optimx-package
  maximal iteration length are 100 simulaiton runs\n")

  #check if packages are installed
  if (!requireNamespace("optimx", quietly = TRUE)) {
    stop("Package optimx needed.")
  }
  if (!requireNamespace("minqa", quietly = TRUE)) {
    stop("Package minqa needed.")
  }
  
  #getting basinInformation to prepare calibration routine
  grdc_number <- basin_object@id
  cont <- (slot(basin_object, "cont")@contName)
  area_info <- basin_object@GAREA
  data_dir <- (slot(basin_object, "cont")@DataDir)
  sim_period_date <- basin_list$SimPeriod
  
  #getting observed discharge for simlation period
  dis <- Q.read_grdc(grdc_number, data_dir,
                    cont, min(sim_period_date), max(sim_period_date))
  dis$Value <- Q.convert_m3s_mmday(dis$Value, sum(area_info)) #mm/day
  df_obs <- dis[(nwarm_up * 365):nrow(dis), ]
  
  calibration.optimRun <- function(parameter_vector) {
    
    #running model
    list2use <- calibration.change_vars(basin_list, parameter_vector)
    wb = runModel(sim_period_date, list2use, settings, nwarm_up)

    #getting Quality
    df = data.frame("Date"= sim_period_date, "Sim" = wb$routing$River$Discharge)
    df <- df[(nwarm_up*365): nrow(df), ]
    targetfunction <- Q.calc_quality(df_obs, df, Type = "QmeanAbs")
    
    return(targetfunction)
  }

  optimizationWGL <- optimx::optimx(par = start_val, fn = calibration.optimRun,
                            gr = NULL, hess = NULL,
                            lower = upper_bound,
                            upper = lower_bound, 
                            method = "bobyqa", 
                            itnmax = 10 * length(start_val)^2, 
                            control = list(maximize=FALSE))
  
  return(optimizationWGL)
}
