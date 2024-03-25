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
// [[Rcpp::export(rng = false)]]
void dailySoil(const NumericVector dailyEffPrec,
			   const NumericVector immediate_runoff,
			   const NumericVector dailySoilPET,
			   const NumericVector dailyCanopyEvapo,
			   const NumericVector dailySnowEvapo,
			   NumericVector G_soilWaterContent,
			   NumericVector dailyAET,
			   NumericVector daily_runoff,
			   NumericVector soil_water_overflow)
{

	double soil_saturation;
	double soil_saturation_with_prec;

	for (int cell = 0; cell < array_size; cell++)
	{

		soil_water_overflow[cell] = 0;
		dailyAET[cell] = 0;

		soil_saturation = G_soilWaterContent[cell] / G_Smax[cell]; //[-]
		soil_saturation_with_prec = (G_soilWaterContent[cell] + dailyEffPrec[cell]) / G_Smax[cell]; // [-]

		if (soil_saturation_with_prec <= lower_threshold_soil)
		{
			daily_runoff[cell] = 0;
		} else {
			daily_runoff[cell] = dailyEffPrec[cell] * pow(soil_saturation, G_GAMMA_HBV[cell]);
		}

		dailyAET[cell] = min(dailySoilPET[cell],
							(maxDailyPET[cell] - dailyCanopyEvapo[cell] - dailySnowEvapo[cell]) * soil_saturation); //formula applied as described in Eisner 2015
		G_soilWaterContent[cell] += dailyEffPrec[cell] - dailyAET[cell] - daily_runoff[cell];

		if (G_soilWaterContent[cell] < 0.)
		{
			dailyAET[cell] += G_soilWaterContent[cell];
			G_soilWaterContent[cell] = 0.;
			soil_water_overflow[cell] = 0.;
		}
		else if (G_soilWaterContent[cell] > G_Smax[cell])
		{
			soil_water_overflow[cell] = G_soilWaterContent[cell] - G_Smax[cell];
			G_soilWaterContent[cell] = G_Smax[cell];
		}
		else
		{
			soil_water_overflow[cell] = 0.;
		}
	}
}
