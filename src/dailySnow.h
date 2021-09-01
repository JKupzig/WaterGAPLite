#ifndef DAILYSNOW_H
#define DAILYSNOW_H

void dailySnow(int day, const NumericVector daily_prec_to_soil, NumericVector G_snow, NumericMatrix G_snowWaterEquivalent,
	NumericVector dailySnowMelt, NumericVector dailySnowEvapo, NumericVector thresh_elev, NumericVector dailyEffPrec,
	NumericVector dailySoilPET);
 
#endif