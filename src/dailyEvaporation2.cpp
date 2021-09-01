#include <Rcpp.h>
#include <math.h>
#include "initModel.h"
#include "dailyEvaporation2.h"
#include "dailyEstimateLongwave.h"

using namespace Rcpp;
using namespace std;

//' @title Calcualting daily potential evapotranspiration
//' @description using Priestley-Taylor approach for calculation of PET
//' @param day day as integer (0 = first day of simulation period)
//' @param Type of PET as string ("water") or other 
//' @param G_snow actual filling of snow storage (to account for snow>3mm --> using snow albedo)
//' @param G_PETnetShort net shortwave radiation 
//' @param G_PETnetLong net longwave radiation 
//' @param DOY Day of the year (1,...,365) - if leap year, 366 is transformed to 365
//' @return PET_day in mm/d
//' @export
///////////////////////////////////////// Potential Evaporation //////////////////////////////////////////////////////////////////////////////


NumericVector dailyEvaporation2(int day, String Type, const NumericVector G_snow, NumericVector G_PETnetShort, NumericVector G_PETnetLong, int DOY){
  
  //const int ncols = Temp.ncol(); //should be equal to array size
  NumericVector albedoToUse (array_size);
  NumericVector PET_day (array_size);
  
  //const double sigma = 0.000000004903; // MJ /(m2 * K4 * day) - Stefan-Boltzmann constant (5.67×10-8 Wm-2 K-4)
  //const double G = 0; // neglected
  //const double gamma = 0.65; // 65 Pa/K Maniak(2015)
  
  
  //creating albedoToUse depending on PET type (water/land)
  if (Type == "water") { 
	for (int i=0; i < array_size; i++){ //does not work!
        albedoToUse[i] = 0.08; //openWaterAlbedo
	}
  } else {
	for (int j=0; j < array_size; j++){
         albedoToUse[j] = albedo[j];
		 if (G_snow[j] > 3.){ //check if there is a mean snow cover > 3mm an use snow albedo than
			 albedoToUse[j] = albedoSnow[j];
		 }
	}
  }
  
  //starting iteration through days and cells
	for (int col = 0; col < array_size; col++){
		

		double dailyShortWave = Rs(day, col); //[W/m2]
		double dailyLongWave = Rl(day,col); //[W/m2]
		double net_long_wave_rad;
			
		//ccalculation scheme form original ModelCode
		double albedo = albedoToUse[col];
		double dailyTempC = Temp(day,col);
		double emissivityCol = emissivity[col]; // land use class dependent emissivity
		double alpha = alphaPT[col];
		double temp_K = dailyTempC + 273.2; // [K]
		const double stefan_boltz_const = 0.000000004903; // MJ /(m2 * K4 * day)
		double lat_heat;
		double dailyPET; 
		
		double temp2 = dailyTempC + 237.3;
		double e_s = 0.6108 * exp(17.27 * dailyTempC / temp2);
		
		if (dailyTempC > 0) { // latent heat of vaporization of water
			lat_heat = 2.501 - 0.002361 * dailyTempC;	// [MJ/kg]
		} else { // latent heat of sublimation
			lat_heat = 2.835;	// 2.501 + 0.334
		} 
		double conv_Wm2_to_mmd = 0.0864 / lat_heat;
		
		double solar_rad = conv_Wm2_to_mmd * dailyShortWave; //[mm/day]
		double net_short_wave_rad = solar_rad * (1. - albedo);
		
		// or estimating it in another way after Kaspar 2004
		if (calcLong == 1) {
			net_long_wave_rad = dailyEstimateLongwave(col, DOY, dailyTempC, dailyShortWave); //mm/d
		} else {
			double long_wave_rad_in = conv_Wm2_to_mmd * dailyLongWave;; // unit: mm/d
			double long_wave_rad_out = emissivityCol * stefan_boltz_const * pow(temp_K, 4.) / lat_heat; // unit: mm/d
			net_long_wave_rad = long_wave_rad_in - long_wave_rad_out; // unit: mm/d
		}
		
				
		double net_rad = net_short_wave_rad + net_long_wave_rad;
		double atmos_pres = 101.3;
		double inc_svp = 4098. * e_s / (temp2 * temp2); 
		double c3 = 0.0016286 * atmos_pres;
		double gamma = c3 / lat_heat;
		
		if (net_rad <= 0.) {
			dailyPET = 0.;
		} else {
			dailyPET = alpha * (inc_svp * net_rad) / (inc_svp + gamma);	// [mm/day]
		}
		PET_day[col] = dailyPET;
		G_PETnetShort[col] = net_short_wave_rad;
		G_PETnetLong[col] = net_long_wave_rad;
	}
		
  // potential evaporation in mm/d
  return(PET_day);
}
