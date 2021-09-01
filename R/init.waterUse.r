#' @title Initializing water use information for model run
#' @description function to initialize water uses 
#' @param basinObject that defines basin for which water use is loaded
#' @param simStart as string of Type \%dd.\%mm.\%YYYY, e.g. 01.01.1980
#' @param simEnd as string of Type \%dd.\%mm.\%YYYY, e.g. 01.01.1990
#' @param WaterUse_Setting 0=noWaterUse is considered and dummy with 0 is loaded, != 1 water use is loaded
#' @return new waterUse object for basin 
#' @export


init.waterUse <- function(basinObject, simStart, simEnd, WaterUse_Setting=0){
  
  DataDir <- slot(basinObject, "cont")@DataDir
  contName <- slot(basinObject, "cont")@contName

  simPeriodDate <- seq(as.Date(simStart, format="%d.%m.%Y"), as.Date(simEnd, format="%d.%m.%Y"),1)
  Years <- as.numeric(unique(format(simPeriodDate, "%Y")))
  
  transMatrix <- basinObject@gcrcWithoutZero
  basIndex <- basinObject@basinIndex
  array_size <- basinObject@array_size
  
  Info_GW <- matrix(0, nrow=length(Years)*12, ncol=array_size)
  Info_SW <- matrix(0, nrow=length(Years)*12, ncol=array_size)
  Info_TF <- matrix(0, nrow=length(Years), ncol=array_size)
  G_NUs_7100 <- rep(0, array_size)
  Info_time <- matrix(0, nrow=length(Years)*12, ncol=2)
  
  if (WaterUse_Setting != 0){
    
    G_NUs_7100 <- readingUNF("G_NUs_7100.UNF0", transMatrix, basIndex, name="waterUse", cont=contName)
    
    i = 1
    for (year in Years){
      
      GW <- sprintf("G_NETUSE_GW_HISTAREA_m3_%d.12.UNF0", year)
      Info_GW[((i-1)*12+1):(i*12),] <- readingUNF(GW, DataDir, transMatrix, basIndex, name="waterUse", cont=contName)
      
      SW <- sprintf("G_NETUSE_SW_HISTAREA_m3_%d.12.UNF0", year)
      Info_SW[((i-1)*12+1):(i*12),] <- readingUNF(SW, DataDir, transMatrix, basIndex, name="waterUse",cont=contName)
      
      TF <- sprintf("G_TRANSFER_DOM_m3_%d.UNF0", year)
      Info_TF[i,] <- readingUNF(TF, DataDir, transMatrix, basIndex, name="waterUse",cont=contName)
      
      Info_time[((i-1)*12+1):(i*12),1] <- rep(year,12)
      Info_time[((i-1)*12+1):(i*12),2] <- seq(1,12,1)
      
      i = i +1
      
    }
  }
  
  newWaterUse <- new("WaterUse", 
                    start=simStart,
                    end=simEnd,
                    Info_GW=Info_GW,
                    Info_SW=Info_SW,
                    Info_TF=Info_TF,
                    G_NUs_7100=G_NUs_7100)

  return(newWaterUse)
  
}
