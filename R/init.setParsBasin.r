#' @title Definition of model parameter values for basinObject
#' @description Function to set parameters of model to basin 
#' @param basinObject that is updated with parameters
#' @param LAI_info table with LAI_info (is then used to define parameters)
#' @param LCT_info table with LCT info (is then used to define parameters)
#' @return basin object with defined parameters  for basin (continent object is part of basin object)
#' @importFrom methods slot<-
#' 
init.SetParsBasin <-function(basinObject, LAI_info, LCT_info) {
  
  parTable <- init.setPars()
  for (i in 6:nrow(parTable)){
    slot(basinObject, parTable[i,1]) <- as.numeric(parTable[i,2])
  }
  
  #LAI table
  GLCT <- basinObject@GLCT
  basinObject@LAI_max <- unlist(LAI_info[GLCT,2],use.names = FALSE)
  #kc_min <- unlist(LAI_info[GLCT,7],use.names = FALSE)
  #kc_max <- unlist(LAI_info[GLCT,6],use.names = FALSE)
  basinObject@initDays <- unlist(LAI_info[GLCT,5],use.names = FALSE)
  red_frDecPlant <- unlist(LAI_info[GLCT,4],use.names = FALSE)
  frDecPlant <- unlist(LAI_info[GLCT,3],use.names = FALSE)
  
  ##LAI_min calculation (original from model code in lai.cpp ll. 93-107)
  lai_factor_a <- 0.1 * frDecPlant
  lai_factor_b <- (1 - frDecPlant) * red_frDecPlant
  basinObject@LAI_min <- lai_factor_a + lai_factor_b * basinObject@LAI_max
  
  #LCT table & arid/humid info
  basinObject@alphaPT <- ifelse(basinObject@G_ARID_HUMID == 1, as.numeric(parTable[1,2]), as.numeric(parTable[2,2])) #are set in function where parameters are set
  basinObject@maxDailyPET <- ifelse(basinObject@G_ARID_HUMID == 1, as.numeric(parTable[3,2]), as.numeric(parTable[4,2])) #are set in function where parameters are set
  basinObject@emissivity <- unlist(LCT_info[GLCT, 6],use.names = FALSE)
  basinObject@albedo <- unlist(LCT_info[GLCT, 3], use.names = FALSE)
  basinObject@albedoSnow <- unlist(LCT_info[GLCT, 4], use.names = FALSE)
  basinObject@rootingDepth <- unlist(LCT_info[GLCT, 2],use.names = FALSE)
  basinObject@degreeDayFactor <- unlist(LCT_info[GLCT, 5],use.names = FALSE)
  
  #SplitFactpr
  basinObject@Splitfactor <- rep(as.numeric(parTable[5,2]), length(GLCT))
                                 
  #Soil informatiom
  if (sum(basinObject@G_BATJES < 0) > 0) {warning("There are negative Values in G_BATJES - these are replaced by 10")}
  basinObject@G_BATJES <- ifelse( basinObject@G_BATJES<0, 10,  basinObject@G_BATJES)
  basinObject@G_Smax = basinObject@G_BATJES * basinObject@rootingDepth
  
  
  #info to simulate/estimate groundwater recharge
  basinObject@G_RG_max = basin.getRGmax(basinObject@G_TEXTURE)
  basinObject@G_gwFactor = basin.getGWfactor(basinObject@G_TEXTURE, basinObject@G_SLOPE_CLASS, basinObject@G_PERMAGLAC, basinObject@G_AQ_FACTOR)
  
  basinObject@landfrac <- 1-(basinObject@G_GLOLAK + basinObject@G_GLOWET + basinObject@G_LOCLAK + basinObject@G_LOCWET)/100 
  
  return(basinObject)
}
