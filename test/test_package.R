library(WaterGAPLite)

#load mock basin
Settings = c(0, # WaterUse --> 0=off, 1=on 2=on (including water transport to cities)
             1, # WaterUseAllocation --> 0: temporal&spatial distribution 1:spatial distribution 2: temporal distribution
             0, # flowVelocity --> 0=const, 1=variable
             0, # GapYear --> 0=With 29.02, 1=Without 29.02 (not used in WGL at the moment)
             1, # reservoirType --> 0: hanasaki, 1: global lakes
             0, # splitting factor --> 0: calculating splitting factor as defined in WG3, 1: setting splitting factor with list (for calibration purpose)
             0, # 0: longwave radiation is read in; 1: Longwave is estimated by incoming shortwave radiaton
             0  # 0: no system values are used, 1: system values are read in, 2: system values are written out, 3: system values are read in and written out
             )



basins =  list( WaterGAPLite::Basin_6340600,
                WaterGAPLite::Basin_1547300,
                WaterGAPLite::Basin_2588200,
                WaterGAPLite::Basin_4147050,
                WaterGAPLite::Basin_4148955,
                WaterGAPLite::Basin_4203410
                )

for (basin in basins){
  print(basin$id)
  wb <- runModel(basin$SimPeriod, basin, Settings, 5)
  df <- data.frame("Date"= basin[["SimPeriod"]], "Sim_mm"=wb$routing$River$Discharge)
  df$Sim = Q.convert_mmday_m3s(df$Sim_mm, sum(basin$GAREA))
  Qobs <- Q.read_grdc(basin$id, NA, NA,  
                     min(basin[["SimPeriod"]]), max(basin[["SimPeriod"]]), 
                     use_folder="C:/Users/jenny/MyProject_sciebo/GRDC_2020/Rohdaten")
  
  
  plot(df$Date,df$Sim, col="maroon", type="l",
       ylim = c(0, max(max(df$Sim), max(Qobs$Value, na.rm=T))))
  lines(Qobs$Date, Qobs$Value, col="cornflowerblue")
  legend( "topleft", inset=0.05,legend=c("Qsim", "Qobs"),
         col=c("maroon", "cornflowerblue"), lty=1, cex=0.8,
         title="legend", text.font=4, bg='grey')
  
  (mgn_l_1 = Q.calcSI(df, func_name="Q.__calc_mgn_l_1__"))
  (mgn_l_2 = Q.calcSI(df, func_name="Q.__calc_mgn_l_2__"))
  (mgn_a_1 = Q.calcSI(df, func_name="Q.__calc_mgn_a_1__"))
  (mgn_a_2 = Q.calcSI(df, func_name="Q.__calc_mgn_a_2__", add_args = sum(basin$GAREA)))
  (mgn_h_1 = Q.calcSI(df, func_name="Q.__calc_mgn_h_1__"))
  (mgn_h_2 = Q.calcSI(df, func_name="Q.__calc_mgn_h_2__", add_args = df$Sim))
  (frq_l_1 = Q.calcSI(df, func_name="Q.__calc_frq_l_1__", add_args = df$Sim)) 
  (frq_l_2 = Q.calcSI(df, func_name="Q.__calc_frq_l_2__", add_args = df$Sim))
  (frq_h_1 = Q.calcSI(df, func_name="Q.__calc_frq_h_1__", add_args = df$Sim)) 
  (frq_h_2 = Q.calcSI(df, func_name="Q.__calc_frq_h_2__", add_args = df$Sim))
  
  (dur_l_1 = Q.calcSI(df, func_name="Q.__calc_dur_l_1__", add_args = df$Sim)) 
  (dur_l_2 = Q.calcSI(df, func_name="Q.__calc_dur_l_2__", add_args = df$Sim))
  (dur_h_1 = Q.calcSI(df, func_name="Q.__calc_dur_h_1__", add_args = df$Sim)) 
  (dur_h_2 = Q.calcSI(df, func_name="Q.__calc_dur_h_2__", add_args = df$Sim))
  
  (timing_1 = Q.calcSI(df, func_name="Q.__calc_timing_1__"))
  (timing_2 = Q.calcSI(df, func_name="Q.__calc_timing_2__", add_args=df))
  (timing_3 = Q.calcSI(df, func_name="Q.__calc_timing_3__"))
  (rchg_1 = Q.calcSI(df, func_name="Q.__calc_rchg_1__"))
  (rchg_2 = Q.calcSI(df, func_name="Q.__calc_rchg_2__"))
}

