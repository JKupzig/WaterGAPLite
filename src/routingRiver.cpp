#include <Rcpp.h>
#include <math.h>
#include "initModel.h"
#include "routingRiver.h"

using namespace Rcpp;
using namespace std;

//' @title routingRiver
//' @description function that defines routing through river (updated)
//' @param cell cell that is simulated
//' @param riverVelocity river velocity in km/d
//' @param RiverInflow inflow to river network [mm*km²/d]
//' @param G_riverOutflow Q_out in [mm*km²/d]
//' @param S_river river storage [mm*km²]
//' @return Q_out in [mm*km²/d]
//' @export
// [[Rcpp::export]]
double routingRiver(
	int cell,
	double riverVelocity,
	double RiverInflow,
	NumericVector G_riverOutflow,
	NumericVector S_river)
	{

    double Q_out;
	double Qout_previous_step;
	double G_riverStoragePrevStep;

	G_riverStoragePrevStep = S_river[cell]; // [mm * km²]
	Qout_previous_step = G_riverOutflow[cell]; // [mm * km² / d]

	Q_out = Qout_previous_step * exp(-1./riverVelocity) // formula after Maniak p. 309 eq. 6.17
			+ (RiverInflow * (1 - exp(-1./riverVelocity)));

	S_river[cell] = G_riverStoragePrevStep + RiverInflow - Q_out;
	G_riverOutflow[cell] = Q_out; 

	return(Q_out);
}

//' @title routingRiverOld
//' @description function that defines routing through river - note: uses original model code with bug in ELS equation
//' @param cell cell that is simulated
//' @param riverVelocity river velocity in km/d
//' @param RiverInflow inflow to river network [mm*km²/d]
//' @param G_riverOutflow transportedVolume in [mm*km²/d]
//' @param S_river river storage [mm*km²]
//' @return Q_out in [mm*km²/d]
//' @export
// [[Rcpp::export]]
double routingRiverOld(
	int cell,
	double riverVelocity,
	double RiverInflow,
	NumericVector G_riverOutflow,
	NumericVector S_river)
	{

    double Q_out;
	double G_riverStoragePrevStep;

	G_riverStoragePrevStep = S_river[cell]; // [mm * km²]

	S_river[cell] = ( G_riverStoragePrevStep * exp(-1./ riverVelocity) )
					+ (RiverInflow * riverVelocity * (1. - exp(-1./riverVelocity)));

	Q_out = RiverInflow + G_riverStoragePrevStep - S_river[cell]; //[mm * km²/d]

	G_riverOutflow[cell] = Q_out;

	return(Q_out);
}


//' @title estimate_pet_from_river
//' @description function that defines routing through river - note: uses original model code with bug in ELS equation
//' @param bankfull_flow_in_cell
//' @param PET in mm
//' @return PET from river [mm*km²]
//' @export
// [[Rcpp::export]]
double estimate_pet_from_river(double bankfull_flow_in_cell, double river_length, double PET)
{
	double riverbed_width = 1./1000. * estimate_bottom_width(bankfull_flow_in_cell); //km
	double bankfullflow_width = 1./1000. * estimate_bankfullflow_width(bankfull_flow_in_cell); //km
	double estimated_surface = (riverbed_width + bankfullflow_width) / 2. * river_length; // km²
	double pet_river = (PET * estimated_surface); // mm*km²
	return(pet_river);
}


//' @title getRiverVelocity
//' @description function that defines river velocity for routing (variable or constant)
//' @param Type 0 (constant) or 1 (variable)
//' @param cell cell that is simulated
//' @param inflow inflow to river in mm*km²/day
//' @return riverVelocity in km/day
//' @export
// [[Rcpp::export]]
double getRiverVelocity(int Type, int cell, double inflow)
{

	double riverVelocity = 0; // [km/day]
	if (Type == 0)
	{
		riverVelocity = defaultRiverVelocity;
	}
	else
	{
		//uses: bankfull flow, river width, river slope
		double riverDepth; //[m]
		double wettedPerimeter;
		double crossSectionalArea;
		double hydraulicRad;
		double incoming_discharge;
		double G_riverBottomWidth;
		double bankfull_flow_in_cell = G_BANKFULL[cell];

		//inflow in mm*km²/day --> m³/sec
		incoming_discharge = (inflow * 1000) / (60.*60.*24.); //[mm*km²/day]  --> [m3/sec]


		// to avoid negative bottom width
		if (bankfull_flow_in_cell < 0.05)
		{
			bankfull_flow_in_cell = 0.05;
		}


		// SE: quick fix to prevent further increase of river velocity at overbank discharges
		// JK: actually it should sink again when exceeding bankfull flow..
		if (incoming_discharge > G_BANKFULL[cell] )
			incoming_discharge = G_BANKFULL[cell];

		// calculate river depth
		riverDepth = 0.349 * pow(incoming_discharge, 0.341);
		G_riverBottomWidth = estimate_bottom_width(bankfull_flow_in_cell);

		// trapezoidal channel shape with channel sides 2:1 run to rise ratio
		crossSectionalArea = riverDepth * (2.0 * riverDepth + G_riverBottomWidth); // trapezoidal river shape
		wettedPerimeter = G_riverBottomWidth + 2.0 * riverDepth * sqrt(5.0); // sqrt(1+2^2)
		hydraulicRad = crossSectionalArea / wettedPerimeter;

		// calculate riverVelocity
		riverVelocity = 1./G_riverRoughness[cell] * pow(hydraulicRad, (2./3.)) * pow(G_riverSlope[cell], 0.5); //[m/sec]
		riverVelocity = riverVelocity * 86.4; //m/sec -->km/day (60*60*24)/1000

		if (riverVelocity < 0.00001)
		{
			riverVelocity = 0.00001;
		}
	}

	return(riverVelocity);
}



//' @title estimate_river_geometry (i.e. bottom width)
//' @description calculate rivers bottom width asssuming a trapezoidal channes with 2/1 run to rise ratio
//' @param cell cell that is simulated
//' @return G_riverBottomWidth in m
//' @export
// [[Rcpp::export]]
double estimate_bottom_width(double bankfull_flow_in_cell)
{
	double G_RiverWidth_bf = estimate_bankfullflow_width(bankfull_flow_in_cell);
	double G_RiverDepth_bf = 0.349 * pow(bankfull_flow_in_cell, 0.341);
	double G_riverBottomWidth = G_RiverWidth_bf - 2.0 * 2.0 * G_RiverDepth_bf;

	return(G_riverBottomWidth);
}


//' @title estimate_river_geometry (i.e.bankfull flow width)
//' @description calculate rivers bankfull flow width asssuming a trapezoidal channes with 2/1 run to rise ratio
//' @param bankfull_flow_in_cell bankfull flow in cell
//' @return G_RiverWidth_bf in m
//' @export
// [[Rcpp::export]]
double estimate_bankfullflow_width(double bankfull_flow_in_cell)
{
	double G_RiverWidth_bf = 2.71 * pow(bankfull_flow_in_cell, 0.557);
	return(G_RiverWidth_bf);
}