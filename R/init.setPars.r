#' @title setting default parameter values for model
#' @description function to define default parameters in model 

init.setPars <- function(){
  
  parTable <- as.data.frame(matrix(NA, ncol=2, nrow=21))
  
  #daily parameters
  parTable[1,]  <- c("alphaArid", 1.74)
  parTable[2,]  <- c("alphaHumid", 1.26)
  parTable[3,]  <- c("maxDailyPET_humid", 10)
  parTable[4,]  <- c("maxDailyPET_arid", 20)
  parTable[5,]  <- c("Splitfactor", 1.0)
  
  parTable[6,]  <- c("maxCanopyStoragePerLAI", 0.3)
  parTable[7,]  <- c("canopyEvapoExp", 2/3)
  parTable[8,]  <- c("runoffFracBuiltUp", 0.5)
  parTable[9,]  <- c("k_g", 0.01)
  
  
  #parTable[9,]  <- c("openWaterAlbedo", 0.08)
  #parTable[10,] <- c("kc_OpenWater", 1.05)
  parTable[10,] <- c("snowFreezeTemp", 0.0)
  parTable[11,] <- c("snowMeltTemp", 0.0)
  #parTable[13,] <- c("windError", 1)
  #parTable[14,] <- c("vapPresError", 1)
  parTable[12,] <- c("pcrit", 12.5) 
  
  #routing parameters
  parTable[13,] <- c("evapoReductionExp", 3.32193) # [-]
  parTable[14,] <- c("evapoReductionExpReservoir", 2.81383) # [-]
  parTable[15,] <- c("wetlandDepth", 0.002) # [km]
  parTable[16,] <- c("lakeDepth", 0.005) # [km]
  parTable[17,] <- c("lakeOutflowExp", 1.5) # [-]
  parTable[18,] <- c("wetlOutflowExp", 2.5) # [-]
  parTable[19,] <- c("loc_storageFactor", 100) # [days]
  parTable[20,] <- c("glo_storageFactor", 80) # [days]
  
  parTable[21,] <- c("defaultRiverVelocity", 86.4) # [km/d]
  
  return(parTable)
  
}

