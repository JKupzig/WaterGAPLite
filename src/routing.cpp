#include <Rcpp.h>
#include <math.h>
#include "routing.h"
#include "ModelTools.h"
#include "initializeModel.h"
#include "initModel.h"
#include "WaterUsePrepareRoutine.h"
#include "routingLocalWaterBodies.h"
#include "routingGlobalLakes.h"
#include "routingGlobalWetlands.h"
#include "routingRiver.h"
#include "routingResHanasaki.h"
#include "WaterUseConsumSW.h"

using namespace Rcpp;
using namespace std;



//' @title routing
//' @description this function includes als routing modules
//' @param SimPeriod Datevector of Simulationperiod
//' @param surfaceRunoff run-off from surface contributing to river network
//' @param GroundwaterRunoff run-off from groundwater contributing to river network
//' @param PETw Potential Evapotranspiration as NumericMatrix in mm/d (to calculate water balance of waterbodies)
//' @param Prec Precipitation as NumericMatrix in mm/d
//' @export
// [[Rcpp::export]]
List routing(
	DateVector SimPeriod,
	NumericMatrix surfaceRunoff,
	NumericMatrix GroundwaterRunoff,
	NumericMatrix PETw,
	NumericMatrix Prec)
	{

	const int n_days = SimPeriod.length();
	const double land_size = sumVector(GAREA);

	Date start_date = SimPeriod[0];
	int start_year = start_date.getYear();

	CheckResType();

	double out_loclake; // mm * km²
	double out_locwet; // mm * km²
	double out_glolake; // mm * km²
	double out_glowet; // mm * km²
	double out_res; // mm * km²
	double inflow_to_river;
	double routed_outflow_from_cell;  // mm*km²
	int cell;
	double inflow_from_upstream; // mm * km²
	int downstream_cell;
	double calculated_river_velocity; // [km/day]

	NumericVector discharge (n_days);
	NumericVector actual_river_velocity (n_days);
	IntegerVector accumulated_days_below_x (array_size);
	NumericVector snow_storage_for_wetland (array_size);

	//states that will be stored
	NumericMatrix discharge_in_river(n_days, array_size);
	NumericMatrix inflow_from_upstream_to_write(n_days, array_size);
	NumericMatrix pet_from_river(n_days, array_size);

	//local Lakes
	NumericMatrix overflow_local_lake(n_days, array_size); // overflow, when S > Smax
	NumericMatrix outflow_local_lake(n_days, array_size);
	NumericMatrix storage_local_lake(n_days, array_size);
	NumericMatrix evaporation_local_lake(n_days, array_size);
	NumericMatrix inflow_local_lake(n_days, array_size);

	//local wetlands
	NumericMatrix overflow_local_wetland(n_days, array_size); // overflow, when S > Smax
	NumericMatrix outflow_local_wetland(n_days, array_size);
	NumericMatrix storage_local_wetland(n_days, array_size);
	NumericMatrix evaporation_local_wetland(n_days, array_size);
	NumericMatrix inflow_local_wetland(n_days, array_size);
    NumericMatrix snow_local_wetland(n_days, array_size);

	//global Lakes
	NumericMatrix overflow_global_lake(n_days, array_size); // overflow, when S > Smax
	NumericMatrix outflow_global_lake(n_days, array_size);
	NumericMatrix storage_global_lake(n_days, array_size);
	NumericMatrix evaporation_global_lake(n_days, array_size);
	NumericMatrix inflow_global_lake(n_days, array_size);

	//reservoirs
	NumericMatrix overflow_reservoir(n_days, array_size);
	NumericMatrix outflow_reservoir(n_days, array_size);
	NumericMatrix storage_reservoir(n_days, array_size);
	NumericMatrix evaporation_reservoir(n_days, array_size);
	NumericMatrix inflow_reservoir(n_days, array_size);


	//global wetlands
	NumericMatrix overflow_global_wetland(n_days, array_size); // overflow, when S > Smax
	NumericMatrix outflow_global_wetland(n_days, array_size);
	NumericMatrix storage_global_wetland(n_days, array_size);
	NumericMatrix evaporation_global_wetland(n_days, array_size);
	NumericMatrix inflow_global_wetland(n_days, array_size);

	//River
	NumericMatrix river_storage(n_days, array_size);

	//WaterUse
	NumericMatrix actual_water_use_surfacewater (n_days, array_size);

	//have to use routing order to be consistent
	numbers =  findUniqueValues(routeOrder);
	numbersSorted = sortIt(numbers);

	for (int day = 0; day < n_days; day++){

		if (day % 100 == 0) {
			Rcpp::checkUserInterrupt();
		}
		Date SimDate = SimPeriod[day];

		// calculate Net Abstraction in mm*km² / day for every cell for groundwater and surface water
		// irrigation is considered as well as transfer of water for bigger cities (domestic)
		int year = SimDate.getYear();
		int month = SimDate.getMonth();
		int day_of_date = SimDate.getDay();

		NumericVector mean_water_demand = WaterUseCalcMeanDemandDaily(year, GapYearType);

		if (GapYearType == 1) { //avoid somulation of the 29.02 to compare modelling result to WG3
			if ((day_of_date == 29) && (month == 2)){
				continue;
			}
		}

		WaterUseCalcDaily(waterUseType, dailyUse, year, month, start_year, Info_GW, Info_SW, Info_TF); // first row = GW, second row = SW

		G_actualUse.fill(0); //clean up actual use from Surface Water Bodies
		G_riverOutflow.fill(0); //to clean up routing network befor starting simulation of actual day

		for (int n = 0; n < numbers.length(); n++){
			int number = numbersSorted[n];
			cellIDs = findNumberInVector(number, routeOrder); // finding cells that have to be calculated
			for (int i=0; i < cellIDs.length(); i++) {
				cell = cellIDs[i];

				const double PrecWater = Prec(day, cell);
				const double PETWater = PETw(day, cell);
				const double TempWater = Temp(day, cell);
				const double LandInflow = (GroundwaterRunoff(day, cell) + surfaceRunoff(day, cell))* GAREA[cell] * landfrac[cell];

				double bankfull_flow_in_cell = G_BANKFULL[cell]; // in ?
				int local_lake_in_cell = G_LOCLAK[cell]; // in percent
				int local_wetland_in_cell = G_LOCWET[cell]; // in percent
				int global_lake_in_cell = G_LAKAREA[cell];
				int reservoir_in_cell = G_RESAREA[cell];
				int global_wetland_in_cell = G_GLOWET[cell];

				out_loclake = LandInflow;
				if (local_lake_in_cell > 0)
				{
					out_loclake = routingLocalWaterBodies(
						0,
						cell,
						PrecWater,
						PETWater,
						TempWater,
						accumulated_days_below_x,
						snow_storage_for_wetland,
						LandInflow,
						S_locLakeStorage,
						locLake_overflow,
						locLake_outflow,
						locLake_evapo,
						locLake_inflow,
						S_locWetlandStorage,
						locWetland_overflow,
						locWetland_outflow,
						locWetland_evapo,
						locWetland_inflow);
				}


				out_locwet = out_loclake;
				if (local_wetland_in_cell > 0)
				{
					out_locwet = routingLocalWaterBodies(
						1,
						cell,
						PrecWater,
						PETWater,
						TempWater,
						accumulated_days_below_x,
						snow_storage_for_wetland,
						out_loclake,
						S_locLakeStorage,
						locLake_overflow,
						locLake_outflow,
						locLake_evapo,
						locLake_inflow,
						S_locWetlandStorage,
						locWetland_overflow,
						locWetland_outflow,
						locWetland_evapo,
						locWetland_inflow);
				}


				inflow_from_upstream = 0.0;
				if (routeOrder[cell] > 1) //if cell is not "head basin" then grap inflow from upstream
				{
					inflow_from_upstream = G_riverOutflow[cell];
					inflow_from_upstream_to_write(day,cell) = G_riverOutflow[cell];
				}


				inflow_to_river = inflow_from_upstream + out_locwet;
				out_glolake = inflow_to_river;
				if (global_lake_in_cell > 0)
				{
					out_glolake = routingGlobalLakes(
						cell,
						PrecWater,
						PETWater,
						inflow_to_river,
						gloLake_overflow,
						gloLake_outflow,
						S_gloLakeStorage,
						gloLake_evapo,
						gloLake_inflow);
				}


				out_res = out_glolake;
				if (reservoir_in_cell > 0)
				{
					out_res = routingResHanasaki(
						day,
						cell,
						SimDate,
						PETWater,
						PrecWater,
						out_glolake,
						Res_outflow,
						Res_overflow,
						S_ResStorage,
						Res_evapo,
						Res_inflow,
						dailyUse,
						mean_water_demand);
				}


				out_glowet = out_res;
				if (global_wetland_in_cell > 0)
				{
					out_glowet =  routingGlobalWetlands(
						cell,
						PrecWater,
						PETWater,
						out_res,
						gloWetland_overflow,
						gloWetland_outflow,
						S_gloWetlandStorage,
						gloWetland_evapo,
						gloWetland_inflow);
				}


				calculated_river_velocity = getRiverVelocity(
					flowVelocityType,
					cell,
					out_glowet); // km/d

				double calculated_duration = G_riverLength[cell] / calculated_river_velocity; // d

				// get evaporation amount
				double pet_river = 0.0f;
				double minimum = 0.0f;
				if (evaporation_from_river == 1)
				{
					pet_river = estimate_pet_from_river(bankfull_flow_in_cell, G_riverLength[cell], PETWater); // mm*km²
				}

				pet_from_river(day, cell) = pet_river;
				double river_in = std::max(out_glowet - pet_river, minimum);

				if (old_river_routing == 1)
				{
					routed_outflow_from_cell = routingRiverOld(
						cell,
						calculated_duration,
						river_in,
						QA_river,
						S_river);
				}
				else
				{
					routed_outflow_from_cell = routingRiver(
						cell,
						calculated_duration,
						river_in,
						QA_river,
						S_river);
				}

				//adding everything to next cell till outlet
				downstream_cell = outflowOrder[cell]-1;
				if (downstream_cell >= 0)
				{
					G_riverOutflow[downstream_cell] += routed_outflow_from_cell;
				}

				//getting routed river in river network
				discharge_in_river(day, cell) = (routed_outflow_from_cell / land_size); //mm;
				if (outflowOrder[cell] < 0)
				{ //end of basin is reached
					discharge[day] = routed_outflow_from_cell / land_size; //mm
					actual_river_velocity[day] = calculated_river_velocity / 86.4 ; // km/d -> m/s
				}
			} // cell loop


			// Abstracting Water Use for surface water
			// river --> reservoir --> global lakes -->local lakes
			if (day_of_date == 1 && month == 1)
			{
				G_totalUnsatisfiedUse.fill(0); //for every year the unsatisfied demand is set to 0
			}
		}

		//subtract water use from cells for surface water bodies and note subtraction in vector
		SubtractWaterConsumSW(
			WaterUseAllocationType,
			dailyUse,
			G_totalUnsatisfiedUse,
			S_river,
			S_ResStorage,
			S_gloLakeStorage,
			S_locLakeStorage,
			G_actualUse);

		//Save all states and fluxes to matrix
		actual_water_use_surfacewater(day,_) = G_actualUse;

		overflow_local_lake(day, _) = locLake_overflow;
		outflow_local_lake(day, _) = locLake_outflow;
		storage_local_lake(day, _) = S_locLakeStorage;
		evaporation_local_lake(day, _) = locLake_evapo;
		inflow_local_lake(day, _) = locLake_inflow;

		overflow_local_wetland(day, _) = locWetland_overflow;
		outflow_local_wetland(day, _) = locWetland_outflow;
		storage_local_wetland(day, _) = S_locWetlandStorage;
		evaporation_local_wetland(day, _) = locWetland_evapo;
		inflow_local_wetland(day, _) = locWetland_inflow;
    	snow_local_wetland(day, _) = snow_storage_for_wetland;

		overflow_global_lake(day, _) = gloLake_overflow;
		outflow_global_lake(day, _) = gloLake_outflow;
		storage_global_lake(day, _) = S_gloLakeStorage;
		evaporation_global_lake(day, _) = gloLake_evapo;
		inflow_global_lake(day, _) = gloLake_inflow;

		outflow_reservoir(day, _) = Res_outflow;
		storage_reservoir(day, _) = S_ResStorage;
		evaporation_reservoir(day, _) = Res_evapo;
		inflow_reservoir(day, _) = Res_inflow;
		overflow_reservoir(day, _) = Res_overflow;

		overflow_global_wetland(day, _) = gloWetland_overflow;
		outflow_global_wetland(day, _) = gloWetland_outflow;
		storage_global_wetland(day, _) = S_gloWetlandStorage;
		evaporation_global_wetland(day, _) = gloWetland_evapo;
		inflow_global_wetland(day, _) = gloWetland_inflow;

		river_storage(day,_) = S_river;

	}

	List result_water_use = List::create(
		Named("ActualUseSW") = actual_water_use_surfacewater);

	List result_river = List::create(
		Named("Discharge") = discharge,
		Named("RiverStorage") = river_storage,
		Named("InflowUpstream") = inflow_from_upstream_to_write,
		Named("RiverAvail") = discharge_in_river,
		Named("RiverInNetwork") = S_river,
		Named("G_riverOutflow") = G_riverOutflow,
		Named("StatVelocity") = actual_river_velocity,
		Named("PETRiver") = pet_from_river);

	List result_local_lake = List::create(
		Named("Overflow") = overflow_local_lake,
		Named("Outflow") = outflow_local_lake,
		 Named("Evapo") = evaporation_local_lake,
		 Named("Storage") = storage_local_lake,
		 Named("Inflow") = inflow_local_lake);

	List result_local_wetland =List::create(
		Named("Overflow") = overflow_local_wetland,
		Named("Outflow") = outflow_local_wetland,
		Named("Evapo") = evaporation_local_wetland,
		Named("Storage") = storage_local_wetland,
		Named("Inflow") = inflow_local_wetland,
		Named("Snow") = snow_local_wetland);

	List result_global_lake = List::create(
		Named("Overflow") = overflow_global_lake,
		Named("Outflow") = outflow_global_lake,
		Named("Evapo") = evaporation_global_lake,
		Named("Storage") = storage_global_lake,
		Named("Inflow") = inflow_global_lake);

	List result_reservoir =List::create(
		Named("Overflow") = overflow_reservoir,
		Named("Outflow") = outflow_reservoir,
		Named("Evapo") = evaporation_reservoir,
		Named("Storage") = storage_reservoir,
		Named("Inflow") = inflow_reservoir);

	List result_global_wetland =List::create(
		Named("Overflow") = overflow_global_wetland,
		Named("Outflow") = outflow_global_wetland,
		Named("Evapo") = evaporation_global_wetland,
		Named("Storage") = storage_global_wetland,
		Named("Inflow") = inflow_global_wetland);


	List routing_result = List::create(
		Named("WaterUseSW") = result_water_use,
		Named("River") = result_river,
		Named("locLake") = result_local_lake,
		Named("locWetland") = result_local_wetland,
		Named("gloLake") = result_global_lake,
		Named("Res") = result_reservoir,
		Named("gloWetland") = result_global_wetland);

	return(routing_result);
}

//' @title CheckResType
//' @description function to simulate reservoirs as global lakes, when res_type is zero, so unknown
//' @export
// [[Rcpp::export]]
void CheckResType(){
	if (ReservoirType == 1)
	{
		for (int cell = 0; cell < array_size; cell++)
		{
			G_LAKAREA[cell] += G_RESAREA[cell];
			G_RESAREA[cell] = 0;
		}
	}
	else
	{
		for (int cell = 0; cell < array_size; cell++)
		{
			if (G_RES_TYPE[cell] == 0)
			{
				G_LAKAREA[cell] += G_RESAREA[cell];
				G_RESAREA[cell] = 0;
			}
		}
	}
}



//' @title setLakeWetlandToMaximum
//' @description function that sets storage of all surface water bodies to max; is implemented in original model version (actually because of this, warm-up-period should be quite long > 2a)
//' @param S_locLakeStorage local lake storage
//' @param S_locWetlandStorage local wetland storage
//' @param S_gloLakeStorage global lake storage
//' @param S_ResStorage reservoir storage
//' @param S_gloWetlandStorage global wetland storage
//' @export
// [[Rcpp::export]]
void setLakeWetlandToMaximum(NumericVector S_locLakeStorage, NumericVector S_locWetlandStorage,
							NumericVector S_gloLakeStorage, NumericVector S_ResStorage,
							NumericVector S_gloWetlandStorage) {

	for (int cell = 0; cell < array_size; cell++) {

		S_locLakeStorage[cell] = (G_LOCLAK[cell] / 100.0) * GAREA[cell] * lakeDepth * 1000 * 1000; //[mm km²]

		S_locWetlandStorage[cell] = (G_LOCWET[cell] / 100.0) * GAREA[cell] * wetlandDepth * 1000 * 1000; //[mm km²]

		S_gloLakeStorage[cell] = G_LAKAREA[cell] * lakeDepth * 1000 * 1000; //[mm km²]

		S_gloWetlandStorage[cell] = (G_GLOWET[cell] / 100.0) * GAREA[cell] * wetlandDepth * 1000 * 1000; //[mm km²]

		S_ResStorage[cell] = G_STORAGE_CAPACITY[cell] * 0.85 * 1000 * 1000;	//[mm km²]
	}
}




