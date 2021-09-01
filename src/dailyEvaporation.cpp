#include <Rcpp.h>
#include <math.h>
#include "initModel.h"
#include "dailyEvaporation.h"

using namespace Rcpp;
using namespace std;


//' @title Calcualting daily potential evapotranspiration
//' @description using simplified Priestley-Taylor approach for calculation of PET (presented in Eisner 2015)
//' @param day as integer (0 = first day of simulation period)
//' @param Type of PET as string ("water") or other 
//' @param G_snow actual filling of snow storage (to account for snow>3mm --> using snow albedo)
//' @return PET_day in mm/d
//' @export
///////////////////////////////////////// Potential Evaporation //////////////////////////////////////////////////////////////////////////////


NumericVector dailyEvaporation(int day, String Type, const NumericVector G_snow){
  
  //const int ncols = Temp.ncol(); //should be equal to array size
  NumericVector albedoToUse (array_size);
  NumericVector PET_day (array_size);
  
  const double sigma = 0.000000004903; // MJ /(m2 * K4 * day) - Stefan-Boltzmann constant (5.67×10-8 Wm-2 K-4)
  const double G = 0; // neglected
  const double gamma = 0.65; // 65 Pa/K Maniak(2015)
  
  
  //creating albedoToUse depending on PET type (water/land)
  if (Type == "water") { 
	for (int i=0; i < array_size; i++){
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
      
      double Rn; // net Radiation in W/m²
      Rn = (1 - albedoToUse[col])*Rs(day,col) + Rl(day,col) - emissivity[col] * sigma * std::pow ((Temp(day,col) + 273.15),4);
      
      double Rn_mm; // net Radiation in mm/d
      Rn_mm = 0.035 * Rn;  
      
      double delta;
      delta = 4098 * (0.6108 * std::exp (17.27 * Temp(day,col) / (Temp(day,col) + 237.3))) / std::pow ((Temp(day,col) + 237.3),2);
      
      // potential evaporation in mm/d
      PET_day[col] = alphaPT[col] * delta/(delta + gamma) * (Rn_mm - G);
  }
  
  return(PET_day);
}
