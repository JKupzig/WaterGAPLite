#include <Rcpp.h>
#include <math.h>

using namespace Rcpp;

//' @title Calculate shortwave radiation
//' @description rcpp function to estimate Shortwave radiation when not given as measured input
//' @param SimDates Datevector of Simulation period
//' @param TempC Temperatur as NumericMatrix in degree
//' @param Sunshine Sunshine duration as NumericMatrix  in hours
//' @param GR information of row for cells
//' @param cor_row information of correction of rows for continental grid
//' @return ShortwaveDownMatrix Matrix with estimated shortwave radiation in W/m²
//' @export
// [[Rcpp::export(rng = false)]]
NumericMatrix dailyEstimateShortwave(DateVector SimDates, NumericMatrix TempC, NumericMatrix Sunshine, IntegerVector GR, int cor_row){
	// after Kaspar 2004 
	
	int array_size= Sunshine.ncol();
	int ndays = Sunshine.nrow();
	NumericMatrix ShortwaveDownMatrix(ndays, array_size);
	
	double dailySunshine;
	Date SimDate;
	int DOY;
	double declination_angle;
	double theta;
	double omega_s;
	double N ;
	double dist_es;
	double ext_rad;
	double ShortwaveDown;
	double dailyTempC;
	double lat_heat;
	double conv_Wm2_to_mmd ;
	
	//constants
	const double a_s = 0.25;	// a_s: fraction of extraterrestrial radiation on overcast days
	const double b_s = 0.5;	    // a_s + b_s: fraction of extraterrestrial radiation on clear days
	const double pi = 3.141592653589793;
	const double pi_180 = pi / 180.0;
	
	const int cellsInDegree=12;
	const double pi2_365 = 2. * pi / 365.0;
	
	for (int col = 0; col < array_size; col++){
		int row =GR[col];
		for (int day=0; day < ndays; day++) { 
			
			dailySunshine = Sunshine(day,col);
			dailyTempC = TempC(day,col);
			
			SimDate = SimDates[day];
			DOY = std::min(SimDate.getYearday(), 365); //1-365 - small differences in computed PET will arrive when leap year (29.02) is neglected in model settings and long/shortwave downward radiation is estimated
			
			if (dailyTempC > 0) { // latent heat of vaporization of water
				lat_heat = 2.501 - 0.002361 * dailyTempC;	// [MJ/kg]
			} else { // latent heat of sublimation
				lat_heat = 2.835;	// 2.501 + 0.334
			}
			conv_Wm2_to_mmd = 0.0864 / lat_heat;
	
			declination_angle = asin(0.39795 * cos(0.2163108 + 2. * atan(0.9671396 * tan(0.00860 * (DOY - 186))))); // solar declination angle (in radians)
			theta= -((row + cor_row) / cellsInDegree - 90. -1./(2.*cellsInDegree)) * pi_180;  // latitude of the site in radians
			omega_s = std::max( std::min( ((sin(theta) * sin(declination_angle)) / (cos(theta) * cos(declination_angle))), 1.) , -1.); // sunset hour angle (in radians) - //eigentlich omega_1, so bezeichnet in dis kaspar A.3
			omega_s = pi - acos(omega_s);//omega_s (stundenwinkel) wird nach kaspars konvention aus omega_1 berechnet
			N = 24/pi * omega_s;	// astronomisch mögliche Sonnenscheindauer nach Forsythe 1995
			
			dist_es = 1. + 0.033 * cos(pi2_365 * DOY); // relative distance earth - sun
			// extraterrestrial radiation [mm/day] - S0
			ext_rad = ( 15.392 * dist_es * (omega_s * sin(theta) * sin(declination_angle) +
					  cos(theta) * cos(declination_angle) * sin(omega_s)) ); //Anhang A.2 Dis Kaspar
			
			ShortwaveDown = (a_s + b_s * std::min(dailySunshine / N, 1.) ) * ext_rad; // mm/d
			ShortwaveDown = ShortwaveDown / conv_Wm2_to_mmd;  // Transformation in W/m²
			ShortwaveDownMatrix(day, col) = ShortwaveDown;
		}
	}
	return(ShortwaveDownMatrix);

}