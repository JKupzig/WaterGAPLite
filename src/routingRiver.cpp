#include <Rcpp.h>
#include <math.h>
#include "initModel.h"
#include "routingRiver.h"

using namespace Rcpp;
using namespace std;

//' @title routingRiver
//' @description function that defines routing through river - note: uses original model code with bug in ELS equation
//' @param cell cell that is simulated
//' @param riverVelocity river velocity in km/d
//' @param RiverInflow inflow to river network [mm*km²/d]
//' @param G_riverOutflow transportedVolume in [mm*km²/d]
//' @param S_river river storage [mm*km²]
//' @return transportedVolume in [mm*km²/d]
//' @export
// [[Rcpp::export]]
double routingRiver(int cell, double riverVelocity, double RiverInflow,
					NumericVector G_riverOutflow, NumericVector S_river) {

	double K;
	double G_riverStoragePrevStep;
	double transportedVolume;

	K = G_riverLength[cell] / riverVelocity; // [km / (km/d)] = [d]
	G_riverStoragePrevStep = S_river[cell]; //[mm * km²]

	S_river[cell] = ( G_riverStoragePrevStep * exp(-1./ K) )
					+ (RiverInflow * K * (1. - exp(-1./K)));

	transportedVolume = RiverInflow + G_riverStoragePrevStep - S_river[cell]; //[mm * km²/d]

	G_riverOutflow[cell] = transportedVolume;

	return(transportedVolume);
}

//' @title getRiverVelocity
//' @description function that defines river velocity for routing (variable or constant)
//' @param Type 0 (constant) or 1 (variable)
//' @param cell cell that is simulated
//' @param inflow inflow to river in mm*km²/day
//' @return riverVelocity in km/day
//' @export
// [[Rcpp::export]]
double getRiverVelocity(int Type, int cell, double inflow){

	double riverVelocity = 0;
	if (Type==0) {
		riverVelocity = defaultRiverVelocity;
	} else {
		//uses: bankfull flow, river width, river slope
		double riverDepth;
		double wettedPerimeter;
		double crossSectionalArea;
		double hydraulicRad;
		double incoming_discharge;

		double G_RiverWidth_bf;
		double G_RiverDepth_bf;
		double G_riverBottomWidth;

		//inflow in mm*km²/day --> m³/sec
		incoming_discharge = (inflow * 1000) / (60.*60.*24.); //[mm*km²/day]  --> [m3/sec]


		// to avoid negative bottom width
		if (G_BANKFULL[cell] < 0.05) {G_BANKFULL[cell] = 0.05; } //not sure why this is set...

		// SE: quick fix to prevent further increase of river velocity at overbank discharges
		// JK: actually it should sink again when exceeding bankfull flow..
		if (incoming_discharge > G_BANKFULL[cell] )
			incoming_discharge = G_BANKFULL[cell];

		// calculate river depth
		riverDepth = 0.349 * pow(incoming_discharge, 0.341); //[m]

		// calculate river bottom width asssuming a trapezoidal channes with 2/1 run to rise ratio
		G_RiverWidth_bf = 2.71 * pow(G_BANKFULL[cell], 0.557); //[m]
		G_RiverDepth_bf = 0.349 * pow(G_BANKFULL[cell], 0.341); //[m]
		G_riverBottomWidth = G_RiverWidth_bf - 2.0 * 2.0 * G_RiverDepth_bf;

		// trapezoidal channel shape with channel sides 2:1 run to rise ratio
		crossSectionalArea = riverDepth * (2.0 * riverDepth + G_riverBottomWidth); // trapezoidal river shape
		wettedPerimeter = G_riverBottomWidth + 2.0 * riverDepth * sqrt(5.0); // sqrt(1+2^2)
		hydraulicRad = crossSectionalArea / wettedPerimeter;


		// calculate riverVelocity
		riverVelocity = 1./G_riverRoughness[cell] * pow(hydraulicRad, (2./3.)) * pow(G_riverSlope[cell], 0.5); //[m/sec]
		riverVelocity = riverVelocity * 86.4; //m/sec -->km/day (60*60*24)/1000

		// return value in [km/day]; if value is below 1cm/day then set limit
		if (riverVelocity < 0.00001){
			riverVelocity = 0.00001;
		}
	}

	return(riverVelocity);
}

