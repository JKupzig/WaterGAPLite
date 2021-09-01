#include <Rcpp.h>
#include <math.h>
#include "daily.h"
#include "initModel.h"
#include "initializeModel.h"
#include "dailyImmediateRunoff.h"
#include "dailyEvaporation2.h"
#include "dailyInterception.h"
#include "dailySnow.h"
#include "dailySoil.h"
#include "dailySplitRunOff.h"
#include "WaterUsePrepareRoutine.h"

using namespace Rcpp;
using namespace std;

//' @title Calculating waterbalance of basin
//' @description {
//' daily routine for each cell
//' this routine is already validated against WG3 simulation for a small basin in South America
//' especially the snow routine and immediate RunOff generation are NOT validated therefore 
//' have to check the simulation results also for other basins to assure a good match with WG3
//' e.g. for european basins with snow processes and sealed areas! (Bayern?) }
//' @param timestring Datevector with dates of simulation period
//' @return List Vwith daily water balance for whole simulaiton period as output
//' @export
// [[Rcpp::export]]
List createWaterBalance(DateVector timestring){
	

	const int ndays = timestring.length();
	Date startDate = timestring[0];
	int startYear = startDate.getYear();
	

	//CREATING OUTPUT
	NumericMatrix Flux_InterceptionEvapo (ndays, array_size);
	NumericMatrix PET (ndays, array_size);
	NumericMatrix PET_netLong (ndays, array_size);
	NumericMatrix PET_netShort (ndays, array_size);
	NumericMatrix PETw (ndays, array_size);
	NumericMatrix Flux_Throughfall( ndays, array_size);
	NumericMatrix Flux_SnowMelt( ndays, array_size);
	NumericMatrix Flux_Sublimation( ndays, array_size);
	NumericMatrix Flux_ImmediateRunoff (ndays, array_size);
	NumericMatrix Flux_dailyAET (ndays, array_size);
	NumericMatrix Flux_dailyRunoff (ndays, array_size);
	NumericMatrix Flux_soilIn (ndays, array_size);
	NumericMatrix Flux_soilWaterOverflow (ndays, array_size);
	NumericMatrix Flux_dailyGWRecharge (ndays, array_size);
	NumericMatrix Flux_dailyLocalSWRunoff (ndays, array_size);
	NumericMatrix Flux_dailyLocalGWRunoff (ndays, array_size);
	NumericMatrix Flux_dailyWaterUseGW (ndays, array_size);
	
	NumericMatrix Storage_CanopyContent (ndays, array_size);
	NumericMatrix Storage_SnowContent (ndays, array_size);
	NumericMatrix Storage_SoilContent (ndays, array_size);
	NumericMatrix Storage_GroundwaterContent (ndays, array_size);
	
	
	for (int time = 0; time < ndays; time++){
		
		Date SimDate = timestring[time];
		int year = SimDate.getYear();
		int month = SimDate.getMonth();
		int dayDate = SimDate.getDay();
		int DOY = min(SimDate.getYearday(), 365); //1-365 - small differences in computed PET will arrive when leap year (29.02) is neglected in model settings and long/shortwave downward radiation is estimated
		// 31 and 30 december is always set to 365 because estimation of longwave and shortwave radiation allows only values between 1-365
		// could be improved in the future that 29.02 is set to 60.5 if it occurs --> own DOY-function needs to be implemented therfore! e.g. https://mariusbancila.ro/blog/2017/08/03/computing-day-of-year-in-c/
		
		if (GapYearType == 1) { //avoid somulation of the 29.02 to compare modelling result to WG3
			if ((dayDate == 29) && (month == 2)){
				continue;
			}
		}
		
		
		//to consider Water Use in Groundwater --> makes model quite slow!
		WaterUseCalcDaily(waterUseType, dailyUse, year, month, startYear, Info_GW, Info_SW, Info_TF);

		
		dailyEffPrec.fill(0); // for every day the effective precipitation flux is set to zero
		dailySnowMelt.fill(0); // for every day the snow melt flux is set to zero
		dailySnowEvapo.fill(0); // for every day the sublimation flux is set to zero
		
		
		//determine PET (because it is also dependend on G_snow)
		NumericVector PETw_day = dailyEvaporation2(time, "water",G_snow, G_PETnetShort,G_PETnetLong, DOY);
		NumericVector PET_day = dailyEvaporation2(time, "land",G_snow,G_PETnetShort,G_PETnetLong, DOY);
		PET(time,_) = PET_day;
		PETw(time,_) = PETw_day;
		PET_netLong(time,_) = G_PETnetLong;
		PET_netShort(time,_) = G_PETnetShort;
		
		
		
		//interception 
		dailyInterception(time, G_canopyWaterContent, 
					   daily_prec_to_soil,  
					   dailySoilPET,
					   dailyCanopyEvapo, PET_day);  //G_canopyWaterContent, daily_prec_to_soil, dailyCanopyEvapo

		Storage_CanopyContent(time,_) = G_canopyWaterContent;
		Flux_Throughfall(time,_) = daily_prec_to_soil;
		Flux_InterceptionEvapo(time,_) = dailyCanopyEvapo;

		
		//snow processes create
		dailySnow(time, daily_prec_to_soil, G_snow, G_snowWaterEquivalent,
					dailySnowMelt, dailySnowEvapo, thresh_elev, dailyEffPrec,
					dailySoilPET);
		Storage_SnowContent(time,_) = G_snow;
		Flux_SnowMelt(time,_) = dailySnowMelt;
		Flux_Sublimation(time,_) = dailySnowEvapo;
		
		//run-off from sealed area --> immediate run-off
		dailyImmediateRunoff(dailyEffPrec, immediate_runoff);
		Flux_ImmediateRunoff(time,_) = immediate_runoff;
		
		//run-off from non-sealed area
		Flux_soilIn(time,_) = dailyEffPrec;
		dailySoil(dailyEffPrec, immediate_runoff,dailySoilPET, 
				dailyCanopyEvapo, dailySnowEvapo, 
				G_soilWaterContent, dailyAET, daily_runoff, soil_water_overflow);
		Storage_SoilContent(time,_) = G_soilWaterContent;
		Flux_dailyAET(time,_) = dailyAET;
		Flux_dailyRunoff(time,_) = daily_runoff;
		Flux_soilWaterOverflow(time,_) = soil_water_overflow;
		
		
		//splitting of run-off
		dailySplitRunOff(time, SimDate, daily_runoff, soil_water_overflow,immediate_runoff,
				daily_gw_recharge, G_groundwater, G_dailyLocalSurfaceRunoff, 
				G_dailyLocalGWRunoff, G_dailyUseGW, dailyUse);
				
		Storage_GroundwaterContent(time,_) = G_groundwater;
		Flux_dailyGWRecharge(time,_) = daily_gw_recharge;
		Flux_dailyLocalSWRunoff(time,_) = G_dailyLocalSurfaceRunoff;
		Flux_dailyLocalGWRunoff(time,_) = G_dailyLocalGWRunoff;
		Flux_dailyWaterUseGW(time, _) = G_dailyUseGW;

	}
	
	List Storages = List::create(Named("CanopyContent") = Storage_CanopyContent, 
								 Named("SnowContent") = Storage_SnowContent, 
								 Named("SoilContent") = Storage_SoilContent, 
								 Named("GroundwaterContent") = Storage_GroundwaterContent);
	
	List Fluxes = List::create(Named("PET") = PET, 
								Named("PETw") = PETw, 
								Named("PET_netLong") = PET_netLong,
								Named("PET_netShort") = PET_netShort,
								
								Named("dailyUse") = dailyUse,
								
								Named("InterceptionEvapo") = Flux_InterceptionEvapo, 
								Named("Throughfall") = Flux_Throughfall,  
								
								Named("Flux_SnowMelt") = Flux_SnowMelt,
								Named("Flux_Sublimation") = Flux_Sublimation,
								
								Named("immediateRunoff") = Flux_ImmediateRunoff,
								
								Named("dailyRunoff") = Flux_dailyRunoff, 
								Named("soilWaterOverflow") = Flux_soilWaterOverflow, 
								Named("Flux_dailyAET") = Flux_dailyAET,
								Named("Flux_soilIn") = Flux_soilIn,
								Named("dailyLocalSWRunoff") = Flux_dailyLocalSWRunoff , 
								Named("dailyLocalGWRunoff") = Flux_dailyLocalGWRunoff, 
								Named("dailyGWRecharge") = Flux_dailyGWRecharge,
								Named("Flux_dailyWaterUseGW") = Flux_dailyWaterUseGW);
				
	List L = List::create(Named("Fluxes") = Fluxes, Named("Storages") = Storages);
	return(L);

}
