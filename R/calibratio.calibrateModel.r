#' @title  Function to calibrate basin
#' @description gamma is changed and gamma value is searched for which mean deviation between simulated and observed discharge is minimal
#' @param BasinObject2Cal BasinObject to be calibrated
#' @param List2use4Cal Model Input for Basin
#' @param Settings Vector for settings to define which model setting should be used to run the model
#' @param nWarmUp number of years that should be used as warm-up, i.e. that should not be considered in model run
#' @param startVal gamma value to start calibration, e.g. c(2.5)
#' @param upperVal upper value for gamma, e.g. c(5.0)
#' @param lowerVal lower value gamma, e.g. c(0.1)
#' @return information about calibration result
#' @export
calibration.calibrateModel <- function(BasinObject2Cal, List2use4Cal, Settings, nWarmUp=5, 
                                       startVal=c(2.5), upperVal=c(0.1), lowerVal=c(5.0)) {
  
  message("CALIBRATION INFO:
  At the moment, only gamma can be varied, 
  function to be optimized is daily absolute mean deviation between simulated and observed discharge
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
  grdc_number <- BasinObject2Cal@id
  cont <- (slot(BasinObject2Cal, "cont")@contName)
  GAREA <- BasinObject2Cal@GAREA
  DataDir <- (slot(BasinObject2Cal, "cont")@DataDir) 
  simPeriodDate <- List2use4Cal$SimPeriod 
  
  #getting observed discharge for simlation period
  dis <- Q.readGRDC(grdc_number, DataDir, cont, min(simPeriodDate), max(simPeriodDate))
  dis$Value <- Q.convert_m3s_mmday(dis$Value , sum(GAREA)) #mm/day
  df_obs = dis
  df_obs <- df_obs[(nWarmUp*365):nrow(df_obs),]
  
  calibration.optimRun <- function(X) { 
    
    #running model
    List2use <- calibration.changeVars(List2use4Cal, X) #BasinObject2Cal has to be in global environment
    wb <- runModel(List2use[["SimPeriod"]], List2use, Settings, 0) 
  
    #getting Quality
    df = data.frame("Date"= simPeriodDate, "Sim"=wb$routing$River$Discharge)
    df <- df[(nWarmUp*365):nrow(df),]
    targetfunction <- Q.calcQuality(df_obs, df, Type="QmeanAbs") #0 = opt
    
    return(targetfunction)
  }

  optimizationWGL <- optimx::optimx(par=startVal, fn=calibration.optimRun, gr=NULL, hess=NULL, 
                            lower=upperVal, 
                            upper=lowerVal, 
                            method="bobyqa", itnmax=10*length(startVal)^2, control=list(maximize=F))
  
  return(optimizationWGL)
}
