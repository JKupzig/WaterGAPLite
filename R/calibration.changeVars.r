#' @title  Function to change model parameters with certain values
#' @description At the moment only Gamma is changed 
#' @param ListConst input List for model run
#' @param X Vector that contains value for gamma to be changed, e.g. c(2.5)
#' @return Changed List that can be used for new model run with different gamma value
#' @export
calibration.changeVars <- function(ListConst, X){
  
  List2return <- ListConst
  BasinSize <- length(List2return[["albedo"]])
  
  #SOIL
  List2return$G_GAMMA_HBV = rep(X[1], BasinSize) #0.01 - 5
  #List2return$G_Smax <- ListConst$G_Smax*X[2] #0.5 - 1.5
  
  #GROUNDWATER
  #List2return$k_g = X[3] #0.001 - 0.1
  #List2return$Splitfactor = ListConst$Splitfactor*interpolate4LHS(LHS_SubSet[13],0.5,1.5)
  
  #RIVER
  #List2return$defaultRiverVelocity = X[4] #50-120
  
  
  return(List2return)
}
