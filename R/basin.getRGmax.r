#' @title Definition of Rg_max
#' @description Function to determin maximal recharge rate for groundwater
#' @param textureInfo information from G_TEXTURE.UNF1
#' @return RG_max as maximal groundwater recharge rate in [mm/d]


basin.getRGmax <- function(textureInfo){
  Rg_max = tools_InterpolateValues(textureInfo, c(10.,15.,20.,25.,30.), c(7.,5.75,4.5,3.5,2.5))
  #These are values for daily simulation (monthly have different values!)
  Rg_max[textureInfo == 0] <- 4.5 #"others"
  Rg_max[textureInfo == 2] <- 4.5 #"others
  Rg_max[textureInfo == 1] <- 0 #rock/glaciers
  if (sum(Rg_max == -999) > 0) {stop("Something is wrong with values in textureInfo - check if all are between 10 and 30 or have 0,1,2 as entries")}
  
  Rg_max <- floor(Rg_max * 100. + 0.5) # float --> short - actually not sure why this is done! --> Ellen
  
  return(Rg_max)
}