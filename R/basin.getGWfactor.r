#' @title Defining Groundwater Recharge Factor
#' @description Function to prepare and get groundwater related parameter (gwFactor = aquiferFactor x permaFactor x slopeFactor x textureFactor)
#' @param textureInfo - information from G_TEXTURE.UNF1
#' @param slopeInfo - information from G_SLOPE_CLASS.UNF1
#' @param permaInfo - information from G_PERMAGLAC.UNF1
#' @param aqiferInfo - information from G_AQ_FACTOR.UNF1
#' @return groundwater factor to determine groundwater recharge


basin.getGWfactor <- function(textureInfo, slopeInfo, permaInfo, aqiferInfo){
  
  aquiferFactor <- aqiferInfo/100. #has to be limited to 0-1
  if ((sum(aquiferFactor > 1) + sum(aquiferFactor < 0)) > 0) {stop("Something is wrong with values in aquiferInfo- check if all are between 0 and 100")}
  
  #permafrost could also be calculated with calculation routine in permafrost.cpp (based on climate info) --> could be used in preprocessing!
  permaFactor   <- 1-(permaInfo/100.)  #has to be limited to 0-1
  if ((sum(permaFactor > 1) + sum(permaFactor < 0)) > 0) {warning("Something is wrong with values in permaInfo - check if all are between 0 and 100")}
  permaFactor[permaFactor < 0] <- 0 #correct values
  permaFactor[permaFactor > 1] <- 1 #correct values
  
  slopeFactor = tools_InterpolateValues(slopeInfo, c(10,20,30,40,50,60,70), c(1.,0.95,0.9,0.75,0.6,0.3,0.15))
  if (sum(slopeFactor == -999) > 0) {stop("Something is wrong with values in SlopeClass - check if all are between 10 and 70")}
  
  textureFactor = tools_InterpolateValues(textureInfo, c(10,15,20,25,30), c(1,0.975,0.95,0.825,0.7))
  textureFactor[textureInfo == 0] <- 0.95 #"others"
  textureFactor[textureInfo == 2] <- 0.95 #"others
  textureFactor[textureInfo == 1] <- 0 #rock/glaciers
  if (sum(textureFactor == -999) > 0) {stop("Something is wrong with values in textureInfo - check if all are between 10 and 30 or have 0,1,2 as entries")}
  
  gwFactor = aquiferFactor * permaFactor * slopeFactor * textureFactor
  
  
  return(gwFactor)
}
