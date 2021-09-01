#include <Rcpp.h>
#include <math.h>
#include "initModel.h"
#include "routingGlobalLakes.h"

//' @title routingGlobalLakes
//' @description function that defines roouting through global lakes
//' @param cell cell that is simulated
//' @param PrecWater Pecipitation above cell [mm]
//' @param PETWater Potential Evaporation form water [mm]
//' @param inflow routed inflow from network to global lake [mm*km²]
//' @param gloLake_overflow overflow from global lake: overflow = (S_gloLakeStorage[cell] - maxStorage);
//' @param gloLake_outflow outflow from global lake: outflow = totalInflow + (storagePrevRouting - S_gloLakeStorage[cell])*G_LAKAREA[cell]; [mm*km²]
//' @param S_gloLakeStorage global lake storage [mm*km²]
//' @param gloLake_evapo evaporation from global lake: evaporation = (PETWater * gloLakeEvapoReductionFactor); [mm*km²]
//' @param gloLake_inflow inflow to global lake: inflow + PrecWater * G_LAKAREA[cell] [mm*km²]
//' @return total outflow form global lake: outflow + overflow [mm*km²]
//' @export
double routingGlobalLakes(int cell, double PrecWater, double PETWater, double inflow,
			NumericVector gloLake_overflow, NumericVector gloLake_outflow , NumericVector S_gloLakeStorage, 
			NumericVector gloLake_evapo, NumericVector gloLake_inflow) {
	
	double maxStorage;
	double totalInflow;
	double outflow;
	double overflow;
	double evaporation;
	double storagePrevRouting;
	
	double gloLakeEvapoReductionFactor;
	
	
	// cell is a natural lake, or the reservoir type is unknown
	// this test can not be realized using 'G_glo_lake'
	// because in some cases 'G_glo_lake' is equal to zero
	// (due to rounding), but 'G_lake_area' has a relevant size
	// because the area of the whole lake is assigned to that cell
	// (which might even be much greater than one cell)

	
	maxStorage = G_LAKAREA[cell] * (lakeDepth * 1000 * 1000); //// maximum storage capacity [mm km²]
	totalInflow = inflow + PrecWater * G_LAKAREA[cell]; //// calculate inflow only [mm km²]


	// This factor was added to reduce PET as a function of actual lake storage (2.1f).
	// Without reduction PET would lead to a continuous decline of lake level in some cases ((semi)arid regions)
	if (S_gloLakeStorage[cell] > maxStorage) {
		gloLakeEvapoReductionFactor = 1.;
	} else {
		gloLakeEvapoReductionFactor = 1. - pow(fabs(S_gloLakeStorage[cell] - maxStorage)
								/ maxStorage, evapoReductionExp);
	}
	
	evaporation = (PETWater * gloLakeEvapoReductionFactor) * G_LAKAREA[cell]; // calculate evaporation from global lakes [mm km²]
	S_gloLakeStorage[cell] -= evaporation; //substract global lake evapo [mm km²]

	// if G_gloLakeStorage is below '0' storage and surfStorageEvapo has to be adjusted
	if (S_gloLakeStorage[cell] < 0.){
		evaporation += S_gloLakeStorage[cell];
		S_gloLakeStorage[cell] = 0;
	}
	
	//know lake storage is stored to apply routing algorithm and to define outflow as difference between storage before and after routing
	storagePrevRouting = S_gloLakeStorage[cell];
	
	// calculate routing through G_gloLakeStorage
	S_gloLakeStorage[cell] = ( storagePrevRouting * exp(-1./glo_storageFactor))
				+ (totalInflow * glo_storageFactor * (1. - exp(-1./glo_storageFactor)));
				
	//G_gloLakeStorage[n] = ( G_gloLakeStoragePrevStep * exp(-1./(((double) timeStepsPerDay) * 1./glo_storageFactor)) )
	//		+ (totalInflow * ((double) timeStepsPerDay) * 1./glo_storageFactor * (1. - exp(-1./(((double) timeStepsPerDay) * 1./glo_storageFactor))) );
				
	outflow = totalInflow + (storagePrevRouting - S_gloLakeStorage[cell]);

	// if global lake is in a cell wich is an inland sink, no outflow occurs
	// and all upstream water is inflow into this global lake
	// (if there are no global wetlands)
	//if (G_LDD[n]==-1) {
	//	if (G_glo_wetland[n] == 0){
	//		G_gloLakeStorage[n] = G_gloLakeStoragePrevStep + totalInflow;
	//		outflow = 0.;
	//	}
	//}

	// reduce G_gloLakeStorage to maximum storage capacity
	if (S_gloLakeStorage[cell] > maxStorage) {
		overflow = (S_gloLakeStorage[cell] - maxStorage);
		S_gloLakeStorage[cell] = maxStorage;
	} else {
		overflow = 0;
	}
	
	
	gloLake_overflow[cell] = overflow;;
	gloLake_outflow[cell] = outflow;
	gloLake_evapo[cell] = evaporation;;
	gloLake_inflow[cell] = totalInflow;
	outflow = outflow + overflow;

	return(outflow);
}