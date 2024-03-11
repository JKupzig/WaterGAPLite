#include <Rcpp.h>
#include <math.h>
#include "initModel.h"
#include "dailySoil.h"

using namespace Rcpp;
using namespace std;

//' @title soil storage implementation
//' @description core of the modle where run-off generation processes are takes part and calibration parameter gamma is implemented
//' @param dailyEffPrec effective precipitation to soil (throughfall + snow melt - fallen snow)
//' @param immediate_runoff immediate run-off that was build over sealed area and does not go into soil storage
//' @param dailySoilPET energy which is left for evapotranspration from soil
//' @param dailyCanopyEvapo evaporation amount of interception (needed to ensure that PET is not > PETdaily,max)
//' @param dailySnowEvapo sublimation amount (needed to ensure that PET is not > PETdaily,max)
//' @param G_soilWaterContent water content of soil storage (<= Smax!)
//' @param dailyAET evapotranspiration from soil storage
//' @param daily_runoff created run-off in soil storage (is later on split into fast and slow component)
//' @param soil_water_overflow overflow of soil storage which contributes directly to the fast run-off component later
//' @export

/////////////////////////////////////////////// SOIL ///////////////////////////////////////////////////////////////////////////


void dailySoil(const NumericVector dailyEffPrec, const NumericVector immediate_runoff, const NumericVector dailySoilPET, 
		  const NumericVector dailyCanopyEvapo, const NumericVector dailySnowEvapo, 
		  NumericVector G_soilWaterContent, NumericVector dailyAET, NumericVector daily_runoff, NumericVector soil_water_overflow){ 
	
	//const int cells = maxDailyPET.length();
	double soil_saturation;
		
	for (int cell = 0; cell < array_size; cell++){
		
		soil_water_overflow[cell] = 0;
		dailyAET[cell] = 0;
		//total_daily_runoff[cell] = 0;
		
		soil_saturation = G_soilWaterContent[cell] / G_Smax[cell]; //[-]
		daily_runoff[cell] = dailyEffPrec[cell] * pow(soil_saturation, G_GAMMA_HBV[cell]);
		// this formula maybe produes NAN in the beginning of a simulation...
		
		//check wether max daily PET should be limited or not (see maxDailyPET_arid or maxDailyPET_humid):
			//Actual evapotranspiration from the soil Ea [mm d-1] is computed as a function of potential evapotranspiration from the soil (Epot − Ec), 
			// the actual soil water content in the effective root zone Ss [mm] and Ss,max: 
			// -->  Epot,max is maximum daily evapotranspiration rate, set to 10 mmd-1 in humid areas and 20 mmd-1 in arid areas. (Eisner, 2015)
			
			//actually not sure why this rate is applied - possibly due to quite high evaporation rates in some areas because of inacuracy of the applied method to estimate PET
		dailyAET[cell] = min(dailySoilPET[cell], (maxDailyPET[cell] - dailyCanopyEvapo[cell] - dailySnowEvapo[cell]) * soil_saturation); //formula applied as described in Eisner 2015
		

		//water balance of the soil
		G_soilWaterContent[cell] += dailyEffPrec[cell] - dailyAET[cell] - daily_runoff[cell];

		if (G_soilWaterContent[cell] < 0.) {
			// too much water has been taken out of the soil water storage --> correction of AET (and run off ?)
			dailyAET[cell] += G_soilWaterContent[cell]; // G_soilWaterContent[n] is negative! THerefore +-
			G_soilWaterContent[cell] = 0.; // correct soil water storage
			soil_water_overflow[cell] = 0.;
		} else if (G_soilWaterContent[cell] > G_Smax[cell]) { //this is a really ugly solution for overflow!
			soil_water_overflow[cell] = G_soilWaterContent[cell] - G_Smax[cell];
			G_soilWaterContent[cell] = G_Smax[cell];
		} else {
			soil_water_overflow[cell] = 0.;
		}
		
		
		//corecction factor is applied afterwards!
		//daily_runoff[cell] *= G_CORR_FACTOR[cell];
		//soil_water_overflow[cell] *= G_CORR_FACTOR[cell];
		//total_daily_runoff[cell] = (daily_runoff[cell] + immediate_runoff[cell] + soil_water_overflow[cell]) * G_CORR_FACTOR[cell];;
		
		// dailyCellLandAET is a corrected actual total evaporation (canopy, snow and soil)
		// it is consistent with cell-corrected runoff
		//dailyCellLandAET[n] = landStorageChangeSum * (G_cellCorrFact[n] - 1.0)
		//		- dailyPrec * (G_cellCorrFact[n] - 1.0)
		//		+ (dailyAET + dailyCanopyEvapo + dailySnowEvapo) * G_cellCorrFact[n];
	}
	//List L = List::create(G_soilWaterContent, dailyAET, daily_runoff, soil_water_overflow);
	//return(L);
}
