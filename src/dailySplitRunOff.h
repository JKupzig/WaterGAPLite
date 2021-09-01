#ifndef DAILYSPLITRUNOFF_H
#define DAILYSPLITRUNOFF_H

void dailySplitRunOff(int day, Date SimDate, const NumericVector daily_runoff, const NumericVector soil_water_overflow, const NumericVector immediate_runoff,
				NumericVector daily_gw_recharge, NumericVector G_groundwater, NumericVector G_dailyLocalSurfaceRunoff, 
				NumericVector G_dailyLocalGWRunoff,NumericVector G_dailyUseGW,  const NumericMatrix dailyUse);
 
#endif