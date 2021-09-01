#include <Rcpp.h>
#include <math.h>
#include "ModelTools.h"
#include "initModel.h"
#include "dailySplitRunOff.h"
#include "WaterUseConsumGW.h"

using namespace Rcpp;
using namespace std;

//' @title Splitting run-off in slow and fast component
//' @description splitting run-off in fast (surface) and slow (groundwater) component using soil information
//' @param day day of simulation period as integer
//' @param SimDate date of day of simulation period as integer
//' @param daily_runoff created run-off in soil storage which is split into fast and slow component
//' @param soil_water_overflow verflow of soil storage which contributes directly to the fast run-off component
//' @param immediate_runoff immediate run-off that was build over sealed area which contributes directly to the fast run-off component
//' @param daily_gw_recharge groundwater recharge rate (input groundwater storage)
//' @param G_groundwater groundwater storage
//' @param G_dailyLocalSurfaceRunoff surface run-off (daily run-off - groundwater recharge rate)+immediate run-off + soil overflow 
//' @param G_dailyLocalGWRunoff outflow of groundwater storage, contributing to net cell-run off (output groundwater storage)
//' @param dailyUse information about water uses 
//' @export

void dailySplitRunOff(int day, Date SimDate, const NumericVector daily_runoff, const NumericVector soil_water_overflow, const NumericVector immediate_runoff,
				NumericVector daily_gw_recharge, NumericVector G_groundwater, NumericVector G_dailyLocalSurfaceRunoff, 
				NumericVector G_dailyLocalGWRunoff,NumericVector G_dailyUseGW, const NumericMatrix dailyUse){ 
	
	const NumericVector dailyPrec = Prec(day,_); 
	double dailyUseGW=0;
	//int year = SimDate.getYear();
	//int month = SimDate.getMonth();
	
	for (int cell = 0; cell < array_size; cell++){
		
		if (splitType == 0) {
			//======================== Groundwater =======================================
			daily_gw_recharge[cell] = min(G_RG_max[cell]/100., G_gwFactor[cell] * daily_runoff[cell]); //geting RG for cell and day 
			//checking special conditions for arid regions
			if ( (G_ARID_HUMID[cell] == 2) & (G_TEXTURE[cell] < 21) & (dailyPrec[cell] < pcrit)){
				daily_gw_recharge[cell] = 0.;      //reducing gw-recharge in arid regions with medium to coarse texture, when there is no heavy rain
			}
			
		} else {
			daily_gw_recharge[cell] = min(G_RG_max[cell]/100.*Splitfactor[cell], G_gwFactor[cell]*Splitfactor[cell]*daily_runoff[cell]); //min(G_RG_max[cell]/100., Splitfactor[cell]*daily_runoff[cell]); //
			//checking special conditions for arid regions
			if ( (G_ARID_HUMID[cell] == 2) & (G_TEXTURE[cell] < 21) & (dailyPrec[cell] < pcrit)){
				daily_gw_recharge[cell] = 0.;      //reducing gw-recharge in arid regions with medium to coarse texture, when there is no heavy rain
			}
		}
		
		//grodunwater routing - would be better with els equation so there is not such a discontinuity
		G_groundwater[cell] += daily_gw_recharge[cell]; //mm
		G_dailyLocalGWRunoff[cell] = max(k_g * G_groundwater[cell], 0.); // so when there is negative groundwater storage than there is no run-off[mm]
		G_groundwater[cell] -= G_dailyLocalGWRunoff[cell]; //mm
		// Actual storage Sb is allowed to fall below 0 as a consequence of an imbalance between abstractions
		//and long-term recharge to mimic the process of groundwater overuse and depletion. (Eisner, 2015)
		
		//Extract/Add Net Abstraction of Groundwater from GW storage
		dailyUseGW = WaterUseConsumGW(cell, G_groundwater, dailyUse) ; //mm --> does not work when dailyUse = 0
		G_dailyUseGW[cell] = dailyUseGW;
		
		// ===================== Surface run-off =====================================
		G_dailyLocalSurfaceRunoff[cell] = immediate_runoff[cell] + soil_water_overflow[cell] + (daily_runoff[cell] - daily_gw_recharge[cell]); 

		// daily_gw_recharge < daily_runoff and all others are positive so values < 0 are not possible
	}
	
	//List L = List::create(G_groundwater, daily_gw_recharge, G_dailyLocalSurfaceRunoff, G_dailyLocalGWRunoff);
	//return(L);
}