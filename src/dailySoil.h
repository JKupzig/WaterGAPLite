#ifndef DAILYSOIL_H
#define DAILYSOIL_H

void dailySoil(const NumericVector dailyEffPrec, const NumericVector immediate_runoff, const NumericVector dailySoilPET, 
		  const NumericVector dailyCanopyEvapo, const NumericVector dailySnowEvapo, 
		  NumericVector G_soilWaterContent, NumericVector dailyAET, NumericVector daily_runoff, NumericVector soil_water_overflow);
 
#endif