#include <Rcpp.h>
#include <math.h>
#include "initModel.h"
#include "routingGlobalWetlands.h"

using namespace Rcpp;
using namespace std;

//' @title routingGlobalWetlands
//' @description function that defines roouting through global wetlands
//' @param cell cell that is simulated
//' @param PrecWater Pecipitation above cell [mm]
//' @param PETWater Potential Evaporation form water [mm]
//' @param inflow routed inflow from network to global wetland [mm*km²]
//' @param gloWetland_overflow overflow from global wetland: overflow = (S_gloWetlandStorage[cell] - maxStorage) [mm*km²]
//' @param gloWetland_outflow outflow form global wetland: outflow = totalInflow + storagePrevRouting - S_gloWetlandStorage[cell] [mm*km²]
//' @param S_gloWetlandStorage global wetland storage [mm*km²]
//' @param gloWetland_evapo from global wetland: evaporation = PETWater * gloWetlEvapoReductionFactor * GAREA[cell] * (G_GLOWET[cell]/100.)[mm*km²]
//' @param gloWetland_inflow inflow to global wetland: inflow + (PrecWater * GAREA[cell] * G_GLOWET[cell]/100) [mm*km²]
//' @return total outflow form global wetland: outflow + overflow [mm*km²]
//' @export
double routingGlobalWetlands(int cell, double PrecWater, double PETWater, double inflow,
		NumericVector gloWetland_overflow, NumericVector gloWetland_outflow, NumericVector S_gloWetlandStorage, 
		NumericVector gloWetland_evapo, NumericVector gloWetland_inflow){
	
	double maxStorage;
	double totalInflow;
	double outflow;
	double overflow;
	double evaporation;
	double storagePrevRouting;
	
	double gloWetlEvapoReductionFactor; // open water PET reduction (2.1f)

	maxStorage = (G_GLOWET[cell] / 100.) * GAREA[cell] * (wetlandDepth * 1000 * 1000); // maximum storage capacity [mm km²]
	totalInflow = inflow + (PrecWater * GAREA[cell] * G_GLOWET[cell]/100); // calculate inflow only [mm km²]

	// This factor was added to reduce PET as a function of actual wetland storage (2.1f).
	if (S_gloWetlandStorage[cell] > maxStorage)
		gloWetlEvapoReductionFactor = 1.;
	else
		gloWetlEvapoReductionFactor = 1. - pow(fabs(S_gloWetlandStorage[cell] - maxStorage)
								/ maxStorage, evapoReductionExp);

	evaporation = PETWater * gloWetlEvapoReductionFactor * GAREA[cell] * (G_GLOWET[cell]/100.); // calculate evaporation from global wetlands [mm*km²]
	S_gloWetlandStorage[cell] -= evaporation;// substract global wetland evapo [mm km²]
						 
	// if G_gloWetlStorage is below '0' storage and surfStorageEvapo has to be adjusted
	if (S_gloWetlandStorage[cell] < 0.){
		evaporation += S_gloWetlandStorage[cell]; // [mm km²]
		S_gloWetlandStorage[cell] = 0.;
	}
	// store value for global lake storage from previous step
	storagePrevRouting = S_gloWetlandStorage[cell];

	// calculate routing through G_gloWetlStorage
	S_gloWetlandStorage[cell] = ( storagePrevRouting * exp(-1./glo_storageFactor) )
				+ (totalInflow * glo_storageFactor * (1. - exp(-1./glo_storageFactor)) );

	outflow = totalInflow + storagePrevRouting - S_gloWetlandStorage[cell];

	// if global wetland is in a cell wich is an inland sink, no outflow occurs
	// and all upstream water is inflow into this global wetland
	//if (G_LDD[n]==-1){
	//	G_gloWetlStorage[n] = storagePrevRouting + totalInflow;
	//	outflow = 0.;
	//}

	// reduce G_gloWetlStorage to maximum storage capacity
	if (S_gloWetlandStorage[cell] > maxStorage) {
		overflow = (S_gloWetlandStorage[cell] - maxStorage);
		S_gloWetlandStorage[cell] = maxStorage;
	} else {
		overflow = 0;
	}
	
	
	gloWetland_overflow[cell] = overflow;;
	gloWetland_outflow[cell] = outflow;
	gloWetland_evapo[cell] = evaporation;;
	gloWetland_inflow[cell] = totalInflow;
	
	outflow = outflow + overflow;
	
	return(outflow);

} // routing_gloWetland