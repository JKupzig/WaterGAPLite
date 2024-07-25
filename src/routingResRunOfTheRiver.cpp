#include <Rcpp.h>
#include <math.h>
#include "ModelTools.h"
#include "initModel.h"
#include "routingResHanasaki2022.h"

using namespace Rcpp;
using namespace std;

double routingResRunOfTheRiver(
    int cell,
    double PETWater,
	double PrecWater,
	double inflow,
    NumericVector Res_outflow,
    NumericVector Res_overflow,
    NumericVector S_ResStorage,
    NumericVector Res_evapo,
    NumericVector Res_inflow)
{
    double Vtotal = G_STORAGE_CAPACITY[cell] * 1000. * 1000.; // [km3] => [mm*km²]
    double totalInflow; 
	double outflow;
	double evaporation;
	double gloResEvapoReductionFactor;
    double overflow = 0.;
    totalInflow = inflow + ( PrecWater * G_RESAREA[cell]);//[mm km²]
    S_ResStorage[cell] += totalInflow;
    if (S_ResStorage[cell] > G_STORAGE_CAPACITY[cell])
		gloResEvapoReductionFactor = 1.;
	else
		gloResEvapoReductionFactor = 1. - pow(fabs(S_ResStorage[cell] - Vtotal)/ Vtotal, evapoReductionExpReservoir);
    // calculate evaporation from global lakes
	evaporation = (PETWater * gloResEvapoReductionFactor)* G_RESAREA[cell]; // [mm km²]

	// substract global lake evapo
	S_ResStorage[cell] -= evaporation; //[mm km²]

	// if G_gloResStorage is below '0' storage and surfStorageEvapo has to be adjusted
	if (S_ResStorage[cell] < 0.){
		evaporation += S_ResStorage[cell];// [mm km²]
		S_ResStorage[cell] = 0.;
	}

    outflow = totalInflow - evaporation;
    S_ResStorage[cell] -= outflow;
    if (S_ResStorage[cell] < 0.){
        outflow += S_ResStorage[cell];
        S_ResStorage[cell] = 0.;
    }
    if (S_ResStorage[cell] > Vtotal){
        overflow = S_ResStorage[cell] - Vtotal;
        S_ResStorage[cell] = Vtotal;
    }

    Res_outflow[cell] = outflow;
    Res_evapo[cell] = evaporation;
    Res_inflow[cell] = totalInflow;
    Res_overflow[cell] = overflow;

    return outflow + overflow;
}