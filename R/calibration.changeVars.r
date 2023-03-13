#' @title  Function to change model parameter Gamma with certain values
#' @description At the moment only Gamma is changed, function can be overwritten to change also other model parameters
#' @param ListConst input List for model run
#' @param X Vector that contains value for gamma to be changed, e.g. c(2.5)
#' @return Changed List that can be used for new model run with different gamma value
#' @export
calibration.changeVars <- function(ListConst, X){
  
  List2return <- ListConst
  BasinSize <- length(List2return[["albedo"]])
  
  List2return$G_GAMMA_HBV = rep(X[1], BasinSize) #0.01 - 5
  
  return(List2return)
}
