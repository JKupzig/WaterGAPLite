#include <Rcpp.h>
#include <math.h>
#include "initModel.h"
#include "dailySnow.h"

using namespace Rcpp;
using namespace std;


//' @title snow storage interpolation
//' @description snow storage is calculated in sub-grid scale (1min) and aggregated to 5min after each day/iteration
//' @param day day of simulation period as integer
//' @param daily_prec_to_soil throughfall from canopy
//' @param G_snow snow storage at 5min scale
//' @param G_snowWaterEquivalent amount of water in snow storage at sub-grid scale
//' @param dailySnowMelt snow melt on 5min cell
//' @param dailySnowEvapo sublimation of snow on 5min cell
//' @param thresh_elev helper - information of reference height,when there is unlimited snow accummulation (> 1000mm)
//' @param dailyEffPrec effective precipitation to soil (throughfall + snow melt - fallen snow)
//' @param dailySoilPET energy for PET which is left for soil
//' @export
// [[Rcpp::export]]
void dailySnow(int day, const NumericVector daily_prec_to_soil, NumericVector G_snow, NumericMatrix G_snowWaterEquivalent,
	NumericVector dailySnowMelt, NumericVector dailySnowEvapo, NumericVector thresh_elev, NumericVector dailyEffPrec,
	NumericVector dailySoilPET){
	
	//parameters to use
	const int subgrids = G_Elevation.nrow();; //1st entry is mean elevation, rest is for subgrids
	//const int cells = G_Elevation.ncol();
	
	//variables to use
	double snowmelt_elev; // snow melt in subgrid
	double temp_elev;	// Temperature in subgrid
	double daily_snow_to_soil_elev; //

	NumericVector dailyTemp = Temp(day,_); //getting Temperature for the day
	
	for (int cell = 0; cell < array_size; cell++){
		
		//loop for all subgrids in cells with elevation >0m. At the end of the loop, snow cover of
		//all subgrids are added to the 0.5° Grid (G_Snow) again.
		for (short elev = 1; elev < subgrids; elev++) {	//count the subgrids
			
			// elevation dependent temperature in one grid cell
			// 0.6°C/100m after Semadeni-Davies (1997) and Dunn, Colohan (1999);
			// The first row of G_ELEV_RANGE.UNF2 (G_Elevation[n][0]) contains mean elevation;
			// actually I chould change G_ELEV_RANGE so there are at least 27 entries with mean elevation also for 0.5° (climate input resolution)
			temp_elev = dailyTemp[cell] - ((G_Elevation(elev,cell) - G_Elevation(0, cell)) * 0.006);
			
			// checking special case to avoid unlimited snow accumulation on glaciers
			// if SWE is > 1000 mm --> 
			if (G_snowWaterEquivalent(elev-1, cell) > 1000.) {

				// define threshold elevation in cell
				if (thresh_elev[cell] == 0.) {
					// first elevation that has snowWaterEquivalent > 1000 is used as threshold elevation 
					// because altitudes are written in increasing order this is the lowest sub scale grid where this case occurs
					// actually I would suggest to use highest sub scale grid where this case does not occur!
					thresh_elev[cell] = G_Elevation(elev, cell); // remember threshold elevation of cell
				} else if (thresh_elev[cell] > 0.) { // cell above threshold elevation
					temp_elev = dailyTemp[cell] - ((thresh_elev[cell] - G_Elevation(0, cell)) * 0.006); // all upper elevations get same temperature calculated with remebered elev
				} 
			}
			
			
			// accumulation of snow and sublimation
			if (temp_elev <= snowFreezeTemp) { //0.0°C

				daily_snow_to_soil_elev = daily_prec_to_soil[cell];
				G_snowWaterEquivalent(elev-1, cell) += daily_snow_to_soil_elev;	//value below canopy

				if (G_snowWaterEquivalent(elev-1, cell) >= dailySoilPET[cell]) {
					G_snowWaterEquivalent(elev-1, cell) -= dailySoilPET[cell];
					dailySnowEvapo[cell] += dailySoilPET[cell];
				} else {
					dailySnowEvapo[cell] += G_snowWaterEquivalent(elev-1, cell);
					G_snowWaterEquivalent(elev-1, cell) = 0.;
				}
			} else {
				dailyEffPrec[cell] += daily_prec_to_soil[cell];} //Precipitation is rain, not snow

			// melting of snow
			if (temp_elev > snowMeltTemp) { //0.0°C
				
				snowmelt_elev = degreeDayFactor[cell] * (temp_elev - snowMeltTemp);

				if (snowmelt_elev > G_snowWaterEquivalent(elev-1, cell)) {
					snowmelt_elev = G_snowWaterEquivalent(elev-1, cell);
					G_snowWaterEquivalent(elev-1, cell) = 0.;
				} else {
					G_snowWaterEquivalent(elev-1, cell) -= snowmelt_elev;
				}
			} else {
				snowmelt_elev = 0;
			}
			
			
			dailySnowMelt[cell] += snowmelt_elev; //snow melt water is stored for potential output
			dailyEffPrec[cell] += snowmelt_elev; // snow melt water is added to effective preiciptation to create run-off later
			G_snow[cell] += G_snowWaterEquivalent(elev-1, cell);	// sum up all snow in subgrids to G_Snow

		} // end subgrids (for)

		G_snow[cell] /= (subgrids-1);
		dailyEffPrec[cell] /= (subgrids-1);	// sum of all subgrids has to be divided by the number of land subgrids
		dailySnowMelt[cell] /= (subgrids-1);	// within the cell (if only land-subgrids, value is 100).	
		dailySnowEvapo[cell] /= (subgrids-1);
		dailySoilPET[cell] -= dailySnowEvapo[cell]; //reducing left evaporation energy
						
  } // end for loop cells
} //end of snow calculations

