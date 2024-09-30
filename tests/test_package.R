library(WaterGAPLite)

#load mock basin
Settings = c(0, # WaterUse --> 0=off, 1=on 2=on (including water transport to cities)
             1, # WaterUseAllocation --> 0: temporal&spatial distribution 1:spatial distribution 2: temporal distribution
             0, # flowVelocity --> 0=const, 1=variable
             0, # GapYear --> 0=With 29.02, 1=Without 29.02 (not used in WGL at the moment)
             1, # reservoirType --> 0: hanasaki, 1: global lakes
             0, # splitting factor --> 0: calculating splitting factor as defined in WG3, 1: setting splitting factor with list (for calibration purpose)
             0, # 0: longwave radiation is read in; 1: Longwave is estimated by incoming shortwave radiaton
             0, # 0: no system values are used, 1: system values are read in, 2: system values are written out, 3: system values are read in and written out
             0) # 0: snow on wetlands off; 1: snow on wetlands on



basins =  list( WaterGAPLite::Basin_1159511,
                WaterGAPLite::Basin_6340600,
                WaterGAPLite::Basin_1547300,
                WaterGAPLite::Basin_2588200,
                WaterGAPLite::Basin_4147050,
                WaterGAPLite::Basin_4148955,
                WaterGAPLite::Basin_4203410
                )

for (basin in basins){
  print(basin$id)
  model_output <- runModel(basin$SimPeriod, basin, Settings, 5)
  testthat::expect_equal(sum(is.na(model_output$routing$River$Discharge)), 0)
  testthat::expect_equal(sum(model_output$routing$River$Discharge < 0), 0)
}

