#include <Rcpp.h>
#include <math.h>

#include "initModel.h"
#include "initializeModel.h"
#include "ModelTools.h"

#include "daily.h"
#include "dailyImmediateRunoff.h"
#include "dailyEvaporation2.h"
#include "dailyInterception.h"
#include "dailySnow.h"
#include "dailySoil.h"
#include "dailySplitRunOff.h"
#include "WaterUsePrepareRoutine.h"

#include "routing.h"
#include "routingLocalWaterBodies.h"
#include "routingGlobalLakes.h"
#include "routingGlobalWetlands.h"
#include "routingRiver.h"
#include "routingResHanasaki.h"
#include "WaterUseConsumSW.h"

using namespace Rcpp;
using namespace std;


void runWarmUp(DateVector timestring, int nYears){
    
	double out_loclake;
	double out_locwet;
	double out_glolake;
	double out_glowet;
	double out_res;
	double RiverInflow;
	double RoutedOutflowCell;
	
	//fill up all waterbodies 
	setLakeWetlandToMaximum(S_locLakeStorage, S_locWetlandStorage, 
							S_gloLakeStorage, S_ResStorage, S_gloWetlandStorage);
	
	//have to use routing order to be consistent 
	numbers =  findUniqueValues(routeOrder); //finding unique values in route order (ascending) => routingSteps
	numbersSorted = sortIt(numbers);
	
	if (nYears <=0) {
		// Rcout << "no warm-up period defined \n";
		return;
	} else {
		// Rcout << "warm-up period of " << nYears << " year(s) used \n";
	}
	
	Date StartDate = timestring[0];
	int StartYear = StartDate.getYear();
	Date LastDate = Date(12,31,StartYear); 
	int DOYofYear = LastDate.getYearday(); //366 or 365
	int ndays = DOYofYear*nYears;
		
	int count=-1;
	for (int time = 0; time < ndays; time++){
		
		count++;
		if (count==DOYofYear){
			count=0; //zurücksetzen auf 1. Tag des Jahres
		}
		
		Date SimDate = timestring[count];
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
		
		
		
		// DAILY ######################################################################
		
		// to consider Water Use in Groundwater --> makes model quite slow!
		WaterUseCalcDaily(waterUseType, dailyUse, year, month, StartYear, Info_GW, Info_SW, Info_TF);

		
		dailyEffPrec.fill(0); // for every day the effective precipitation flux is set to zero
		dailySnowMelt.fill(0); // for every day the snow melt flux is set to zero
		dailySnowEvapo.fill(0); // for every day the sublimation flux is set to zero
		
		
		//determine PET (because it is also dependend on G_snow)
		NumericVector PETw_day = dailyEvaporation2(count, "water",G_snow, G_PETnetShort,G_PETnetLong, DOY);
		NumericVector PET_day = dailyEvaporation2(count, "land",G_snow,G_PETnetShort,G_PETnetLong, DOY);

		//interception 
		dailyInterception(count, G_canopyWaterContent, 
					   daily_prec_to_soil,  
					   dailySoilPET,
					   dailyCanopyEvapo, PET_day);  //G_canopyWaterContent, daily_prec_to_soil, dailyCanopyEvapo

		
		//snow processes create
		dailySnow(count, daily_prec_to_soil, G_snow, G_snowWaterEquivalent,
					dailySnowMelt, dailySnowEvapo, thresh_elev, dailyEffPrec,
					dailySoilPET);
		
		//run-off from sealed area --> immediate run-off
		dailyImmediateRunoff(dailyEffPrec, immediate_runoff);
		
		//run-off from non-sealed area
		dailySoil(dailyEffPrec, immediate_runoff,dailySoilPET, 
				dailyCanopyEvapo, dailySnowEvapo, 
				G_soilWaterContent, dailyAET, daily_runoff, soil_water_overflow);
		
		//splitting of run-off
		dailySplitRunOff(count, SimDate, daily_runoff, soil_water_overflow,immediate_runoff,
				daily_gw_recharge, G_groundwater, G_dailyLocalSurfaceRunoff, 
				G_dailyLocalGWRunoff, G_dailyUseGW, dailyUse);
				
		
		// ROUTING ######################################################################
		
		NumericVector MeanDemand = WaterUseCalcMeanDemandDaily(year, GapYearType);
		WaterUseCalcDaily(waterUseType, dailyUse, year, month, StartYear, Info_GW, Info_SW, Info_TF); // first row = GW, second row = SW
		
		G_actualUse.fill(0); //clean up actual use from Surface Water Bodies
		G_riverOutflow.fill(0); //to clean up routing network befor starting simulation of actual day
		
		for (int n = 0; n < numbers.length(); n++){
			int number = numbersSorted[n];
			cellIDs = findNumberInVector(number, routeOrder); //finding cells that have to be calculated in actual routing step -> starts from 0
			for (int i=0; i < cellIDs.length(); i++) {
				int cell = cellIDs[i]; 
				float InflowUpstream = 0.0;
				
				
				const double PrecWater = Prec(count, cell);
				const double PETWater = PETw_day[cell]; //calculated in daily above
				const double LandInflow = (G_dailyLocalGWRunoff[cell] + G_dailyLocalSurfaceRunoff[cell])* GAREA[cell] * landfrac[cell]; // mm * km²
				
				//routing process within cell - could also be part of waterbalance or otherwise - ground water routing could also be part of routing!
				// local lakes
				if (G_LOCLAK[cell] > 0) {
					out_loclake = routingLocalWaterBodies(0, cell, PrecWater, PETWater, LandInflow,
								S_locLakeStorage, locLake_overflow, locLake_outflow, locLake_evapo, locLake_inflow,
								S_locWetlandStorage, locWetland_overflow, locWetland_outflow, locWetland_evapo, locWetland_inflow); // mm * km²
				} else {
					out_loclake = LandInflow; // mm * km²
				}
				
				//local wetlands
				if (G_LOCWET[cell] > 0) {
					out_locwet = routingLocalWaterBodies(1, cell, PrecWater, PETWater, out_loclake,
								S_locLakeStorage, locLake_overflow, locLake_outflow, locLake_evapo, locLake_inflow,
								S_locWetlandStorage, locWetland_overflow, locWetland_outflow, locWetland_evapo, locWetland_inflow); 
				} else {
					out_locwet = out_loclake; // mm * km²
				}
				

				//if cell is not "head basin" then grap inflowFrom Upstream Information
				if (routeOrder[cell] > 1){
					InflowUpstream = G_riverOutflow[cell];
				}	
				
				RiverInflow = InflowUpstream + out_locwet; // mm * km²
				
				//global lakes
				if (G_LAKAREA[cell] > 0) {
					out_glolake = routingGlobalLakes(cell, PrecWater, PETWater, RiverInflow,
								  gloLake_overflow, gloLake_outflow , S_gloLakeStorage, 
								  gloLake_evapo,  gloLake_inflow); // mm * km²
				} else {
					out_glolake = RiverInflow; // mm * km²
				}
				
				//reserviors
				if (G_RESAREA[cell] > 0) {
					out_res = routingResHanasaki(count, cell, SimDate, PETWater, PrecWater, out_glolake, 
							Res_outflow, Res_overflow, S_ResStorage, Res_evapo, Res_inflow,
							dailyUse, MeanDemand);
				} else {
					out_res = out_glolake;
				}
				
				// global wetlands 
				if (G_GLOWET[cell] > 0) {
					out_glowet =  routingGlobalWetlands(cell, PrecWater, PETWater, out_res,
								 gloWetland_overflow, gloWetland_outflow, S_gloWetlandStorage, 
								 gloWetland_evapo, gloWetland_inflow);
				} else {
					out_glowet = out_res;
				}
				
				//river segment
				float riverVelocity = getRiverVelocity(flowVelocityType, cell, out_glowet); // 0 = const, other=variable [km/day]
				RoutedOutflowCell = routingRiver(cell, riverVelocity, out_glowet, 
									QA_river, S_river); // mm*km²
				
				//adding everything to next cell till outlet
				int downstreamcell = outflowOrder[cell]-1;
				if (downstreamcell >= 0){
					G_riverOutflow[downstreamcell] += RoutedOutflowCell;
				}
			} // cell loop
			
			
			//Abstracting Water Use for surface water 
			// river --> reservoir --> global lakes -->local lakes
			if (dayDate==1 && month == 1) {
				G_totalUnsatisfiedUse.fill(0); //for every year the unsatisfied demand is set to 0
			}
		}
		
		//subtract water use from cells for surface water bodies and note subtraction in vector
		SubtractWaterConsumSW(WaterUseAllocationType, dailyUse, G_totalUnsatisfiedUse,
						   S_river, S_ResStorage, S_gloLakeStorage, 
						   S_locLakeStorage,G_actualUse);
	}
	return;
}