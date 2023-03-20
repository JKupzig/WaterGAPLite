#' @title  Function to change model parameter Gamma with certain values
#' @description At the moment only Gamma is changed, function can be overwritten to change also other model parameters
#' @param basin_list input List for model run
#' @param parameter_vector Vector that contains value for gamma to be changed, e.g. c(2.5)
#' @return Changed List that can be used for new model run with different gamma value
#' @export
calibration.change_vars <- function(basin_list, parameter_vector) {

  calibrated_list <- basin_list
  array_size <- length(calibrated_list[["albedo"]])

  calibrated_list$G_GAMMA_HBV <- rep(parameter_vector[1], array_size) 
  return(calibrated_list)
}
