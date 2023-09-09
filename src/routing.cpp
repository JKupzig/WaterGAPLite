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

//have to check function for open water bodies - especially for reservoirs...
// variable flow velocity works
// water use is not validatet at the moment

//help functions to use from ModelTools:
//IntegerVector findNumberInVector(int number, IntegerVector vec);
//IntegerVector findUniqueValues(IntegerVector vec);
//IntegerVector sortIt(IntegerVector vec);
//double sumVector(NumericVector vec);
//int numberOfDaysInMonth(int month, int year);
//int numberOfDaysInYear(int year);


void CheckResType();

void setLakeWetlandToMaximum(NumericVector S_locLakeStorage, NumericVector S_locWetlandStorage,
							NumericVector S_gloLakeStorage, NumericVector S_ResStorage,
							NumericVector S_gloWetlandStorage);


//' @title routing
//' @description this function includes als routing modules
//' @param SimPeriod Datevector of Simulationperiod
//' @param surfaceRunoff run-off from surface contributing to river network
//' @param GroundwaterRunoff run-off from groundwater contributing to river network
//' @param PETw Potential Evapotranspiration as NumericMatrix in mm/d (to calculate water balance of waterbodies)
//' @param Prec Precipitation as NumericMatrix in mm/d
//' @export
// [[Rcpp::export]]
List routing(DateVector SimPeriod, NumericMatrix surfaceRunoff, NumericMatrix GroundwaterRunoff,
			NumericMatrix PETw, NumericMatrix Prec){

	const int ndays = SimPeriod.length();
	const double landSize = sumVector(GAREA); //basinArea (only landfraction is considered)

	Date startDate = SimPeriod[0];
	int startYear = startDate.getYear();

	CheckResType();

	//is now done in runWarmUp()
	//setLakeWetlandToMaximum(S_locLakeStorage, S_locWetlandStorage,
	//						S_gloLakeStorage, S_ResStorage, S_gloWetlandStorage);

	double out_loclake;
	double out_locwet;
	double out_glolake;
	double out_glowet;
	double out_res;
	double RiverInflow;
	double RoutedOutflowCell;

	NumericVector Discharge (ndays);
	NumericVector RiverVelocityStat (ndays);
  NumericVector AccumDegreeDays (array_size);
  NumericVector SnowStorageWetland (array_size);


	//Zustände die gespeichert werden
	NumericMatrix RiverAvail(ndays, array_size); //
	NumericMatrix InflowUpstream2write(ndays, array_size); //


	//local Lakes
	NumericMatrix OverflowlocLake(ndays, array_size); // special overflow, when S > Smax
	NumericMatrix OutflowlocLake(ndays, array_size);  // total outflow
	NumericMatrix StoragelocLake(ndays, array_size);  // storage of lake
	NumericMatrix EvapolocLake(ndays, array_size);    // Evaporatiom from Lake
	NumericMatrix InflowlocLake(ndays, array_size);   // Inflow to Lake

	//local wetlands
	NumericMatrix OverflowlocWetland(ndays, array_size); // special overflow, when S > Smax
	NumericMatrix OutflowlocWetland(ndays, array_size);  // total outflow
	NumericMatrix StoragelocWetland(ndays, array_size);  // storage of Wetland
	NumericMatrix EvapolocWetland(ndays, array_size);    // Evaporatiom from Wetland
	NumericMatrix InflowlocWetland(ndays, array_size);   // Inflow to Wetland
    NumericMatrix SnowlocWetland(ndays, array_size);   // Snow Wetland

	//global Lakes
	NumericMatrix OverflowgloLake(ndays, array_size); // special overflow, when S > Smax
	NumericMatrix OutflowgloLake(ndays, array_size);  // total outflow
	NumericMatrix StoragegloLake(ndays, array_size);  // storage of lake
	NumericMatrix EvapogloLake(ndays, array_size);    // Evaporatiom from Lake
	NumericMatrix InflowgloLake(ndays, array_size);   // Inflow to Lake

	//reservoirs
	NumericMatrix OutflowRes(ndays, array_size);  // total outflow
	NumericMatrix StorageRes(ndays, array_size);  // storage of Reservoir
	NumericMatrix EvapoRes(ndays, array_size);    // Evaporatiom from Reservoir
	NumericMatrix InflowRes(ndays, array_size);   // Inflow to Reservoir  Res_overflow
	NumericMatrix OverflowRes(ndays, array_size); // overflow from Reservoir when precipitation above Reservoir and Inflow are to high

	//global wetlands
	NumericMatrix OverflowgloWetland(ndays, array_size); // special overflow, when S > Smax
	NumericMatrix OutflowgloWetland(ndays, array_size);  // total outflow
	NumericMatrix StoragegloWetland(ndays, array_size);  // storage of Wetland
	NumericMatrix EvapogloWetland(ndays, array_size);    // Evaporatiom from Wetland
	NumericMatrix InflowgloWetland(ndays, array_size);   // Inflow to Wetland

	//River
	NumericMatrix RiverStorage(ndays, array_size); // special overflow, when S > Smax

	//WaterUse
	NumericMatrix ActualUseSW (ndays, array_size);

	int cell;
	double InflowUpstream;
	int downstreamcell;
	double riverVelocity;

	//have to use routing order to be consistent
	numbers =  findUniqueValues(routeOrder); //finding unique values in route order (ascending) => routingSteps
	numbersSorted = sortIt(numbers);

	for (int day = 0; day < ndays; day++){

		if (day % 100 == 0) {
			Rcpp::checkUserInterrupt(); // check for interrupt every 100 iterations
		}
		Date SimDate = SimPeriod[day];

		//calculate Net Abstraction in mm*km² / day for every cell for groundwater and surface water
		// irrigation is considered as well as transfer of water for bigger cities (domestic)
		int year = SimDate.getYear();
		int month = SimDate.getMonth();
		int dayDate = SimDate.getDay();

		NumericVector MeanDemand = WaterUseCalcMeanDemandDaily(year, GapYearType);

		if (GapYearType == 1) { //avoid somulation of the 29.02 to compare modelling result to WG3
			if ((dayDate == 29) && (month == 2)){
				continue;
			}
		}

		WaterUseCalcDaily(waterUseType, dailyUse, year, month, startYear, Info_GW, Info_SW, Info_TF); // first row = GW, second row = SW

		G_actualUse.fill(0); //clean up actual use from Surface Water Bodies
		G_riverOutflow.fill(0); //to clean up routing network befor starting simulation of actual day

		for (int n = 0; n < numbers.length(); n++){
			int number = numbersSorted[n];
			cellIDs = findNumberInVector(number, routeOrder); //finding cells that have to be calculated in actual routing step -> starts from 0
			for (int i=0; i < cellIDs.length(); i++) {
				cell = cellIDs[i];
				InflowUpstream = 0.0;


				const double PrecWater = Prec(day, cell);
				const double PETWater = PETw(day, cell);
				const double TempWater = Temp(day, cell);
				const double LandInflow = (GroundwaterRunoff(day, cell) + surfaceRunoff(day, cell))* GAREA[cell] * landfrac[cell]; // mm * km²

				//routing process within cell - could also be part of waterbalance or otherwise - ground water routing could also be part of routing!
				// local lakes
				if (G_LOCLAK[cell] > 0) {
					out_loclake = routingLocalWaterBodies(0, cell, PrecWater, PETWater, TempWater, AccumDegreeDays, SnowStorageWetland, LandInflow,
								S_locLakeStorage, locLake_overflow, locLake_outflow, locLake_evapo, locLake_inflow,
								S_locWetlandStorage, locWetland_overflow, locWetland_outflow, locWetland_evapo, locWetland_inflow); // mm * km²
				} else {
					out_loclake = LandInflow; // mm * km²
				}

				//local wetlands
				if (G_LOCWET[cell] > 0) {
					out_locwet = routingLocalWaterBodies(1, cell, PrecWater, PETWater, TempWater, AccumDegreeDays, SnowStorageWetland, out_loclake,
								S_locLakeStorage, locLake_overflow, locLake_outflow, locLake_evapo, locLake_inflow,
								S_locWetlandStorage, locWetland_overflow, locWetland_outflow, locWetland_evapo, locWetland_inflow);
				} else {
					out_locwet = out_loclake; // mm * km²
				}


				// water that comes out of the system of local lakes/wetlands
				// is inflow into the river and is being routed through global lakes and wetlands

				//if cell is not "head basin" then grap inflowFrom Upstream Information
				if (routeOrder[cell] > 1){
					InflowUpstream = G_riverOutflow[cell];
					InflowUpstream2write(day,cell) = G_riverOutflow[cell];
				}


				RiverInflow = InflowUpstream + out_locwet; // mm * km²

				//global lakes
				if (G_LAKAREA[cell] > 0) {
					out_glolake = routingGlobalLakes(cell, PrecWater, PETWater, RiverInflow,
								  gloLake_overflow, gloLake_outflow , S_gloLakeStorage,
								  gloLake_evapo, gloLake_inflow); // mm * km²
				} else {
					out_glolake = RiverInflow; // mm * km²
				}

				//reserviors
				if (G_RESAREA[cell] > 0) {
					out_res = routingResHanasaki(day, cell, SimDate, PETWater, PrecWater, out_glolake,
							Res_outflow, Res_overflow, S_ResStorage, Res_evapo, Res_inflow,
							dailyUse, MeanDemand);
				} else {
					out_res = out_glolake;
				}

				//out_res = out_glolake;

				// global wetlands
				if (G_GLOWET[cell] > 0) {
					out_glowet =  routingGlobalWetlands(cell, PrecWater, PETWater, out_res,
								 gloWetland_overflow, gloWetland_outflow, S_gloWetlandStorage,
								 gloWetland_evapo, gloWetland_inflow);
				} else {
					out_glowet = out_res;
				}

				//river segment
				riverVelocity = getRiverVelocity(flowVelocityType, cell, out_glowet); // 0 = const, other=variable [km/day]
				RoutedOutflowCell = routingRiver(cell, riverVelocity, out_glowet,
									QA_river, S_river); // mm*km²

				//adding everything to next cell till outlet
				downstreamcell = outflowOrder[cell]-1;
				if (downstreamcell >= 0){
					G_riverOutflow[downstreamcell] += RoutedOutflowCell;
				}

				//getting routed river in river network
				RiverAvail(day, cell) = (RoutedOutflowCell / landSize); //mm;
				if (outflowOrder[cell] < 0) { //end of basin is reached
					Discharge[day] = RoutedOutflowCell / landSize; //mm
					RiverVelocityStat[day] = riverVelocity / 86.4 ; // m/s
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
		ActualUseSW(day,_) = G_actualUse;

		//Save all states and fluxes to matrix
		OverflowlocLake(day, _) = locLake_overflow; // mm*km² -> (GAREA * G_LOCLAK / 100.); //mm
		OutflowlocLake(day, _) = locLake_outflow; // mm*km² - >(GAREA * G_LOCLAK / 100.); //mm
		StoragelocLake(day, _) = S_locLakeStorage; // mm*km²- >(GAREA * G_LOCLAK / 100.); //mm
		EvapolocLake(day, _) = locLake_evapo; // mm*km²- >(GAREA * G_LOCLAK / 100.); //mm
		InflowlocLake(day, _) = locLake_inflow; // mm*km²- >(GAREA * G_LOCLAK / 100.); //mm

		OverflowlocWetland(day, _) = locWetland_overflow; // mm*km²- >(GAREA * G_LOCWET / 100.); //mm
		OutflowlocWetland(day, _) = locWetland_outflow; // mm*km²- >(GAREA * G_LOCWET / 100.); //mm
		StoragelocWetland(day, _) = S_locWetlandStorage; // mm*km²- >(GAREA * G_LOCWET / 100.); //mm
		EvapolocWetland(day, _) = locWetland_evapo; // mm*km²- >(GAREA * G_LOCWET / 100.); //mm
		InflowlocWetland(day, _) = locWetland_inflow; // mm*km²- >(GAREA * G_LOCWET / 100.); //mm
    SnowlocWetland(day, _) = SnowStorageWetland; //mm

		OverflowgloLake(day, _) = gloLake_overflow; // mm*km²- >(GAREA * G_LOCWET / 100.); //mm
		OutflowgloLake(day, _) = gloLake_outflow; // mm*km²- >(GAREA * G_LOCWET / 100.); //mm
		StoragegloLake(day, _) = S_gloLakeStorage; // mm*km²- >(GAREA * G_LOCWET / 100.); //mm
		EvapogloLake(day, _) = gloLake_evapo; // mm
		InflowgloLake(day, _) = gloLake_inflow; // mm*km²- >(GAREA * G_LOCWET / 100.); //mm

		OutflowRes(day, _) = Res_outflow; // (GAREA * G_LOCWET / 100.); //mm
		StorageRes(day, _) = S_ResStorage; // (GAREA * G_LOCWET / 100.); //mm
		EvapoRes(day, _) = Res_evapo; // (GAREA * G_LOCWET / 100.); //mm
		InflowRes(day, _) = Res_inflow; // (GAREA * G_LOCWET / 100.); //mm
		OverflowRes(day, _) = Res_overflow; // (GAREA * G_LOCWET / 100.); //mm

		OverflowgloWetland(day, _) = gloWetland_overflow; // mm*km²- >(GAREA * G_LOCWET / 100.); //mm
		OutflowgloWetland(day, _) = gloWetland_outflow; // mm*km²- >(GAREA * G_LOCWET / 100.); //mm
		StoragegloWetland(day, _) = S_gloWetlandStorage; // mm*km²- >(GAREA * G_LOCWET / 100.); //mm
		EvapogloWetland(day, _) = gloWetland_evapo; // mm
		InflowgloWetland(day, _) = gloWetland_inflow; // mm*km²- >(GAREA * G_LOCWET / 100.); //mm

		RiverStorage(day,_) = S_river;

	}

	List WaterUseSW = List::create(Named("ActualUseSW") = ActualUseSW);
	List River = List::create(Named("Discharge") = Discharge, Named("RiverStorage")=RiverStorage, Named("InflowUpstream")=InflowUpstream2write, Named("RiverAvail") = RiverAvail, Named("RiverInNetwork")= S_river, Named("G_riverOutflow")= G_riverOutflow, Named("StatVelocity") = RiverVelocityStat);
	List locLake = List::create(Named("Overflow") = OverflowlocLake, Named("Outflow") = OutflowlocLake, Named("Evapo") = EvapolocLake, Named("Storage") = StoragelocLake, Named("Inflow") = InflowlocLake);
	List locWetland =List::create(Named("Overflow") = OverflowlocWetland, Named("Outflow") = OutflowlocWetland, Named("Evapo") = EvapolocWetland, Named("Storage") = StoragelocWetland, Named("Inflow") = InflowlocWetland, Named("Snow")=SnowlocWetland);
	List gloLake = List::create(Named("Overflow") = OverflowgloLake, Named("Outflow") = OutflowgloLake, Named("Evapo") = EvapogloLake, Named("Storage") = StoragegloLake, Named("Inflow") = InflowgloLake);
	List Res =List::create(Named("Overflow") = OverflowRes, Named("Outflow") = OutflowRes, Named("Evapo") = EvapoRes, Named("Storage") = StorageRes, Named("Inflow") = InflowRes);
	List gloWetland =List::create(Named("Overflow") = OverflowgloWetland, Named("Outflow") = OutflowgloWetland, Named("Evapo") = EvapogloWetland, Named("Storage") = StoragegloWetland, Named("Inflow") = InflowgloWetland);


	List L = List::create(Named("WaterUseSW") = ActualUseSW, Named("River") = River,
			Named("locLake") = locLake, Named("locWetland") = locWetland,
			Named("gloLake") = gloLake, Named("Res") = Res ,
			Named("gloWetland") = gloWetland);
	return(L);
}

//' @title CheckResType
//' @description function to simulate reservoirs as global lakes, when res_type is zero, so unknown
//' @export
// [[Rcpp::export]]
void CheckResType(){
	if (ReservoirType == 1){
		for (int cell = 0; cell < array_size; cell++) {
			G_LAKAREA[cell] += G_RESAREA[cell]; //use reservoir area as lake area, or sum up reservoir and lake area
			G_RESAREA[cell] = 0; //set reservoir area to zero
		}
	} else {
		for (int cell = 0; cell < array_size; cell++) {
			if (G_RES_TYPE[cell] == 0) {// IF reservoir type is UNKNOWN
				G_LAKAREA[cell] += G_RESAREA[cell]; //use reservoir area as lake area, or sum up reservoir and lake area
				G_RESAREA[cell] = 0; //set reservoir area to zero
				// have to think about option to write out the affected cells
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

//double probe(int cell, int downstreamcell, double K, double Inflow){
//
//	double RoutedInflow;
//	//
//	NumericVector QA_river = Environment::global_env()["QA_river"]; //has always river outflow from previous time step
//	NumericVector QZ_river = Environment::global_env()["QZ_river"]; //has always river inflow from previous time step
//	NumericVector G_riverOutflow = Environment::global_env()["G_riverOutflow"]; //has always river inflow from previous time step
//
//	// ELS equation (analytical solution) with assumption of linear inflow
//	RoutedInflow = QA_river[cell-1] + (Inflow - QA_river[cell-1]) * (1-exp(-1/K)) +
//					(Inflow - QZ_river[cell-1]) * (1-K * (1-exp(-1/K)));
//	QA_river[cell-1] = RoutedInflow; //update values for next time step
//	QZ_river[cell-1] = Inflow;
//
//
//	G_riverOutflow[downstreamcell-1] = RoutedInflow; //überschreiben klappt nicht?
//
//
//	return(RoutedInflow);
//
//}



