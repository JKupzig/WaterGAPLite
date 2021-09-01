#include <Rcpp.h>
#include <math.h>
#include "initModel.h"
#include "WaterUseConsumGW.h"

//' @title WaterUseConsumGW
//' @description function that abstracts water use from groundwater storage 
//' @param cell cell in basin that is used for abstraction
//' @param GroundwaterStorage groundwater storage level (can be negative due to abstraction)
//' @param dailyUse information of water that needs to be abstracted from groundwater (first row of NumericMatrix)
//' @return GWdailyuse abstracted groundwater (if there is no landfraction in cell, no water can be abstracted from groundwater)
//' @export
// [[Rcpp::export]]
double WaterUseConsumGW(int cell, NumericVector GroundwaterStorage, const NumericMatrix dailyUse) {
	
	double GWdailyuse;
	//double dailyUseVal = dailyUse(0, cell); --> this does not work on my work PC
	if ((GAREA[cell] > 0) && (landfrac[cell] > 0)) { // to avoid division through zero
		// convert unit mm*km²/day to unit mm/day (G_groundwater[n])
		GWdailyuse = dailyUse.at(0, cell) / (GAREA[cell] * landfrac[cell]);
		GroundwaterStorage[cell] -= GWdailyuse;
	} else {
		GWdailyuse = 0.0;
	}
	return(GWdailyuse);
}