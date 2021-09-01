#include <Rcpp.h>
#include <math.h>
#include "initModel.h"
#include "dailyEstimateLongwave.h"

//' @title Calculate longwave radiation
//' @description estimation of longwave radiation when it is not given as measured variable
//' @param n index of specific cell
//' @param DOY day of the year (between 1 and 365)
//' @param dailyTempC Temperature of Day in Degree
//' @param dailyShortWave shortwave radiation as double in W/m²
//' @return net_long_wave_rad net longwave radiation in W/m²
//' @export
// [[Rcpp::export(rng = false)]]
double dailyEstimateLongwave(int n, int DOY, double dailyTempC, double dailyShortWave){
	// need also form initModel: NumericVector G_AridHumid, GR, cor_row
	// after Kaspar 2004 
	
	const double a_c_arid	= 1.35;	// long-wave radiation coefficients for clear skies
	const double b_c_arid 	= -0.35;	// sum has to be 1.0
	const double a_c_humid 	= 1.00;
	const double b_c_humid 	= 0.00;
	const double a_s = 0.25;	// a_s: fraction of extraterrestrial radiation on overcast days
	const double b_s = 0.5;	    // a_s + b_s: fraction of extraterrestrial radiation on clear days
	const double pi = 3.141592653589793;
	const double pi_180 = pi / 180.0;
	const double pi2_365 = 2. * pi / 365.0;
	const double stefan_boltz_const = 0.000000004903;	// MJ /(m2 * K4 * day)
	const double cellsInDegree = 12;
	
	double a_c=a_c_arid; //to make sure that a_c and b_c have a valid when there is other entry then 1 or 2 in G_ARID_HIMUD.UNF
	double b_c=b_c_arid;
	double lat_heat;
	
	// pre-defined arid-humid areas 
	switch (G_ARID_HUMID[n]) {
	case 2: // arid area
		a_c = a_c_arid;
		b_c = b_c_arid;
		break;
	case 1: // humid area
		a_c = a_c_humid;
		b_c = b_c_humid;
	}
	
	int row =GR[n];
	
	// getting necessarily climatlogical information (Temp and Shortwave)
	double net_emissivity = -0.02 + 0.261 * exp(-0.000777 * dailyTempC * dailyTempC); // net emissivity between the atmosphere and the ground
	double temp_K = dailyTempC + 273.2;	// [K]
	
	// latent heat of evaporation
	if (dailyTempC > 0) { // latent heat of vaporization of water
		lat_heat = 2.501 - 0.002361 * dailyTempC;	// [MJ/kg]
	} else { // latent heat of sublimation
		lat_heat = 2.835;	// 2.501 + 0.334
	}
	double conv_Wm2_to_mmd = 0.0864 / lat_heat; // about 0.034 for T = 0C and 0.0345 for T = 10C, 0.0352 for T = 20C

	// solar declination angle (in radians)
	double declination_angle = asin(0.39795 * cos(0.2163108 + 2. * atan(0.9671396 * tan(0.00860 * (DOY - 186)))));
	// latitude of the site in radians
	double theta= -((row + cor_row) / cellsInDegree - 90. -1./(2.*cellsInDegree)) * pi_180;  //changed for WaterGAP3
	// sunset hour angle (in radians) - //eigentlich omega_1, so bezeichnet in dis kaspar A.3
	double omega_s = max( min( ((sin(theta) * sin(declination_angle)) / (cos(theta) * cos(declination_angle))), 1.) , -1. ); //gl(A.8)
	omega_s = pi - acos(omega_s);//omega_s (stundenwinkel) wird nach kaspars konvention aus omega_1 berechnet
	// relative distance earth - sun
	double dist_es = 1. + 0.033 * cos(pi2_365 * DOY);
	// extraterrestrial radiation [mm/day]
	double ext_rad = ( 15.392 * dist_es * (omega_s * sin(theta) * sin(declination_angle) +
			  cos(theta) * cos(declination_angle) * sin(omega_s)) ); //Anhang A.2 Dis Kaspar
		
	double solar_rad = conv_Wm2_to_mmd * dailyShortWave; //[mm/day]
	double solar_rad_0 = (a_s + b_s) * ext_rad;	// mm/day =S0
	
	// adjustment for cloud cover
	double solar_fraction;
	if (ext_rad <= 0.)
		solar_fraction = 0.;
	else
		solar_fraction = min(solar_rad / solar_rad_0, 1.); // this case can happen if solar_rad is derived from measured data and solar_rad_0 is calculated

	double ff = a_c * solar_fraction + b_c;
	
	double net_long_wave_rad = ( -ff * net_emissivity * stefan_boltz_const * pow(temp_K,4.) ) / lat_heat; // [mm/day]
	
	return(net_long_wave_rad);

}