
root <- r"(C:\Users\jenny\MyProject_sciebo\WaterGAPlite)"
setwd(root)

Settings = c(0, # WaterUse --> 0=off, 1=on 2=on (including water transport to cities)
             1, # WaterUseAllocation --> 0: temporal&spatial distribution 1:spatial distribution 2: temporal distribution
             0, # flowVelocity --> 0=const, 1=variable
             0, # GapYear --> 0=With 29.02, 1=Without 29.02 (not used in WGL at the moment)
             1, # reservoirType --> 0: hanasaki, 1: global lakes
             0, # splitting factor --> 0: calculating splitting factor as defined in WG3, 1: setting splitting factor with list (for calibration purpose)
             0, # 0: longwave radiation is read in; 1: Longwave is estimated by incoming shortwave radiaton
             0  # 0: no system values are used, 1: system values are read in, 2: system values are written out, 3: system values are read in and written out
)

start = "01.01.1980"
end   = "31.12.1982" 
basinData <- read.csv(r"(C:\Users\jenny\MyProject_sciebo\_SensitvityAnalysis\CaseStudy\Basins\overviewBasins.txt)")

i <- which(basinData$grdc_no == 6340600)

bas <- init.model(grdc_number=basinData[i,2], 
                  lat=basinData[i,5],
                  long=basinData[i,4],
                  cont=as.character(basinData[i,7]),
                  root)

bas.climate <- init.climate(basin_object = bas,
                            start = start, 
                            end = end,
                            force2read = TRUE,
                            climate_format = "global")

bas.waterUse <- init.wateruse(basin_object = bas, 
                              sim_start = start, 
                              sim_end = end, 
                              wateruse_setting=Settings[1])

bas.model <- basin.prepare_run(basin_object = bas, 
                               climate_object = bas.climate, 
                               wateruse_object = bas.waterUse) 

model_run <- runModel(bas.model$SimPeriod, bas.model, Settings, 3)

plot(bas.model$SimPeriod, model_run$routing$River$Discharge, type="l")
