#include <Rcpp.h>
#include <math.h>
#include "initModel.h"
#include "routingLocalWaterBodies.h"

using namespace Rcpp;
using namespace std;

//' @title routingGlobalWetlands
//' @description function that defines routing through local waterbodies
//' @param Type 0 (local lake) or 1 (local wetland)
//' @param cell cell that is simulated
//' @param PrecWater Pecipitation above cell [mm]
//' @param PETWater Potential Evaporation form water [mm]
//' @param TempWater Mean Temperature above cell [mm]
//' @param accum_days accumulated days below -10 degree[mm]
//' @param Inflow inflow to local wb from cell=(GroundwaterRunoff(day, cell) + surfaceRunoff(day, cell))* GAREA[cell] * landfrac[cell] or (for the case of local wetlands) outflow form local lake in the case of local wetland and local lake are present [mm*km²]
//' @param S_locLakeStorage local lake storage [mmm*km²]
//' @param locLake_overflow overflow of local lake (locStorage[cell] - maxStorage) [mm*km²]
//' @param locLake_outflow outflow of local lake (only via routing, overflow is extra) [mm*km²]
//' @param locLake_evapo evaporation from local lake PETWater * locEvapoReductionFactor * (GAREA[cell] * locPerc[cell] / 100.) [mm*km²]
//' @param locLake_inflow total inflow to local lake Inflow + PrecWater * (GAREA[cell] * locPerc[cell] / 100.); // mm km²
//' @param S_locWetlandStorage local wetland storage [mm*km²]
//' @param locWetland_overflow overflow of local wetland (locStorage[cell] - maxStorage) [mm*km²]
//' @param locWetland_outflow outflow of local wetland (only via routing, overflow is extra) [mm*km²]
//' @param locWetland_evapo evaporation from local wetland PETWater * locEvapoReductionFactor * (GAREA[cell] * locPerc[cell] / 100.) [mm*km²]
//' @param locWetland_inflow inflow to local wetland Inflow + PrecWater * (GAREA[cell] * locPerc[cell] / 100.); // mm km²
//' @return routed outflow from local waterbody [mm*km²]
//' @export
// [[Rcpp::export]]
double routingLocalWaterBodies(bool Type, int cell, double PrecWater,  double PETWater, double TempWater, IntegerVector accum_days, NumericVector snow_storage_wetland, double Inflow,
								NumericVector S_locLakeStorage, NumericVector locLake_overflow, NumericVector locLake_outflow, NumericVector locLake_evapo, NumericVector locLake_inflow,
								NumericVector S_locWetlandStorage, NumericVector locWetland_overflow, NumericVector locWetland_outflow, NumericVector locWetland_evapo, NumericVector locWetland_inflow) {


	double totalInflow;
	double outflow;
	double maxStorage;
	double locEvapoReductionFactor; // open water PET reduction (2.1f)
	double surfStorageEvapo = 0.; // added for cell AET calculation (WG3.1)

	//double snow_threshold = -5;
	//double max_degree_days = 10;
	double snowmelt;


	//prepare everything for Type definition:
	//const IntegerVector locPerc = Environment::global_env()["G_LOCLAK"]; // % of cell that belongs to local lake
	IntegerVector locPerc(array_size, -99);
	double Depth;
	double OutflowExp;

	NumericVector locStorage (array_size);
	NumericVector locOverflow (array_size);
	NumericVector locOutflow (array_size);
	NumericVector locEvapo (array_size);
	NumericVector locInflow (array_size);

	// variables dependent on type (lake or wetland)
	if (Type == 0) { // I think, this takes quite a time in the function
		locPerc = G_LOCLAK; // % of cell that belongs to local lake
		Depth = lakeDepth; // 0.005 km --> 5000 mm
		OutflowExp = lakeOutflowExp; // 1.5 [-]

		locStorage = S_locLakeStorage;
		locOverflow = locLake_overflow;
		locOutflow = locLake_outflow;
		locEvapo = locLake_evapo;
		locInflow = locLake_inflow;

	} else if (Type == 1) {
		locPerc = G_LOCWET; // % of cell that belongs to local wetland
		Depth = wetlandDepth; // 0.002 km --> 2000 mm
		OutflowExp = wetlOutflowExp; // 2.5 [-]

		locStorage = S_locWetlandStorage;
		locOverflow = locWetland_overflow;
		locOutflow = locWetland_outflow;
		locEvapo = locWetland_evapo;
		locInflow = locWetland_inflow;

		if (snowInWetland == 1) {

			// snow module for wetlands
			if (TempWater <= snow_threshold) {
				accum_days[cell] += 1.;
				accum_days[cell] = min(accum_days[cell], max_degree_days);
			} else {
				accum_days[cell] -= 1.;
				accum_days[cell] = max(accum_days[cell], 0);
			}

			if (accum_days[cell] == max_degree_days) { //frozen wetland

				// accumulation of snow
				if (TempWater <= snowFreezeTemp) { //0.0°C
					snow_storage_wetland[cell] += PrecWater;
					PrecWater = 0;
				}
			}

			// melting of snow
			if (TempWater > snowMeltTemp) { //0.0°C
				snowmelt = min(4.0 * abs(TempWater - snowMeltTemp), snow_storage_wetland[cell]);
				snow_storage_wetland[cell] -= snowmelt;
				PrecWater += snowmelt;

			}
		}
	}

	// maximum storage capacity --> threshold for lake (logical?)
	maxStorage = (locPerc[cell] / 100. * GAREA[cell]) * (Depth * 1000 * 1000.);	// [mm km²]
	// This factor was added to reduce PET as a function of actual lake storage (2.1f).
	// Without reduction PET would lead to a continuous decline of lake level in some cases ((semi)arid regions)
	if (locStorage[cell] > maxStorage) {
		locEvapoReductionFactor = 1.;
	} else {
		locEvapoReductionFactor = 1. - pow((fabs(locStorage[cell] - maxStorage) / (maxStorage)), evapoReductionExp);
	}




	//calculate water balance from lake
	surfStorageEvapo = PETWater * locEvapoReductionFactor * (GAREA[cell] * locPerc[cell] / 100.); // calculate evaporation from local lakes mm km²
	totalInflow = Inflow + PrecWater * (GAREA[cell] * locPerc[cell] / 100.); // mm km²

	// original model code:
	// 1) Evaporation is substracted
	locStorage[cell] -= surfStorageEvapo; 	// substract local lake evapo
	// 2) Storage is proofed and maybe corrected
	if (locStorage[cell] < 0.){ // if G_locLakeStorage is below '0', evaporation is reduced and there is no outflow anymore
		surfStorageEvapo +=  locStorage[cell];  /// (GAREA[cell] / 1000000.) / (G_LOCLAK[cell] / 100.);
		locStorage[cell] = 0.;
	}

	// 3) add inflow to storage : PROBLEM: STORAGE CAN BE NOW BIGGER THAN MAX!
	locStorage[cell] += totalInflow;
	// 4) calculate routing through G_locLakeStorage --> not stable for cases with locStorage > MaxStorage!
	outflow = (1./loc_storageFactor) * locStorage[cell] * pow((locStorage[cell] / maxStorage), OutflowExp); // mm km²


	// if local lake is in a cell wich is an inland sink, no outflow occurs
	// (if there are no local wetlands and no global lakes/wetlands)
	//if (G_LDD[n]==-1) {
	//	if ( ((G_glo_lake[n] + G_glo_wetland[n]) == 0) && (G_loc_wetland[n]==0) ){
	//		outflow = 0.;
	//	}
	//}

	// 5) substract outflow from storage
	locStorage[cell] -= outflow;

	// 6) reduce G_locLakeStorage to maximum storage capacity
	if (locStorage[cell] > maxStorage) {
		locOverflow[cell] = (locStorage[cell] - maxStorage);
		outflow += (locStorage[cell] - maxStorage);
		locStorage[cell] = maxStorage;
	// 7) check if outflow created was to big --> negative storage needs to be avoided!
	} else if (locStorage[cell] < 0.) { // needed because upper solution is not stable for S > Smax!! --> outflow > S occurs
		outflow += locStorage[cell];
		locStorage[cell] = 0.;
		locOverflow[cell] = 0.0;
	} else {
		locOverflow[cell] = 0.0;
	}

	locEvapo[cell] = surfStorageEvapo; // mm km²
	locInflow[cell] = totalInflow; // mm km²
	locOutflow[cell] = outflow - locOverflow[cell]; // mm km²

	// now saving all again to adequate vectors - NEW
	if (Type == 0) {
		S_locLakeStorage[cell] = locStorage[cell];
		locLake_overflow[cell] = locOverflow[cell];
		locLake_outflow[cell]  = locOutflow[cell];
		locLake_evapo[cell]    = locEvapo[cell];
		locLake_inflow[cell]   = locInflow[cell];
	} else if (Type == 1) {
		S_locWetlandStorage[cell] = locStorage[cell];
		locWetland_overflow[cell] = locOverflow[cell];
		locWetland_outflow[cell]  = locOutflow[cell];
		locWetland_evapo[cell]    = locEvapo[cell];
		locWetland_inflow[cell]   = locInflow[cell];
	}

	return(outflow);
}
