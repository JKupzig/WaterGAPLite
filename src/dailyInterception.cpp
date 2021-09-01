#include <Rcpp.h>
#include <math.h>
#include "initModel.h"
#include "dailyInterception.h"

using namespace Rcpp;
using namespace std;


//' @title Interception storage implementation
//' @description calculation of interception storage with variable interception storage 
//' @param day day of simulation period as integer
//' @param G_canopyWaterContent interception storage 
//' @param daily_prec_to_soil throughfall from canopy
//' @param dailySoilPET energy for PET which is left for soil or snow storage
//' @param dailyCanopyEvapo Evaporation from interception storage
//' @param dailyPET total energy for PET on this specific day
//' @export
///////////////////////////////////////// Interception //////////////////////////////////////////////////////////////////////////////

void dailyInterception(int day, NumericVector G_canopyWaterContent, NumericVector daily_prec_to_soil,  NumericVector dailySoilPET,
		NumericVector dailyCanopyEvapo, const NumericVector dailyPET){
   
  //const int cells = G_canopyWaterContent.length();
  double max_canopy_storage;
  double canopy_deficiency;
  double canopy_water_content; 
  
  NumericVector dailyLAI = dailyLaiAll(day,_); //getting Interception Storage for the day
  NumericVector dailyPrec = Prec(day,_); //getting Precipitation for the day
  
  for (int cell = 0; cell < array_size; cell++){
  
  // calculation of LAI has been moved before calculations of albedo starts
	if ((dailyLAI[cell] > 0.00001) & (maxCanopyStoragePerLAI > 0)) { //if there is interception storage available, maxCanopyStoragePerLAI can be used to turn interception off
		max_canopy_storage = maxCanopyStoragePerLAI * dailyLAI[cell];	// [mm]
		canopy_deficiency = max_canopy_storage - G_canopyWaterContent[cell]; //space left in interception storage, due to variable storage negative values are possible
		if (dailyPrec[cell] < canopy_deficiency) {
		  G_canopyWaterContent[cell] += dailyPrec[cell];
		  daily_prec_to_soil[cell] = 0.;
		} else { //Throughfall to soil
		  G_canopyWaterContent[cell] = max_canopy_storage;
		  daily_prec_to_soil[cell] = dailyPrec[cell] - canopy_deficiency;
		}
		
		//calculation of evapotranspiration from interception storage
		canopy_water_content = G_canopyWaterContent[cell];
		dailyCanopyEvapo[cell] = dailyPET[cell] * pow((canopy_water_content / max_canopy_storage), canopyEvapoExp); // canopyEvapoExp = 2/3
		if (dailyCanopyEvapo[cell] > canopy_water_content) {
		  // All the water in the canopy is evaporated. dailyCanopyEvapo has to be reduced, because
		  // part of the energy is left and can lead to additional evapotranspiration from soil later in the program.
		  dailyCanopyEvapo[cell] = canopy_water_content;
		  dailySoilPET[cell] = dailyPET[cell] - canopy_water_content;
		  G_canopyWaterContent[cell] = 0.0;
		} else {
		  G_canopyWaterContent[cell] -= dailyCanopyEvapo[cell];
		  dailySoilPET[cell] = dailyPET[cell] - dailyCanopyEvapo[cell];
		}
	} else { //no interception storage is available
		daily_prec_to_soil[cell] = dailyPrec[cell];
		dailySoilPET[cell] = dailyPET[cell];
		dailyCanopyEvapo[cell] = 0.0;
	}
  }
  //List L = List::create(G_canopyWaterContent, daily_prec_to_soil, dailyCanopyEvapo);
  //return(L);
}
