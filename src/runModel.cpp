#include <Rcpp.h>
#include "initModel.h"
#include "initializeModel.h"
#include "initialStorages.h"
#include "daily.h"
#include "routing.h"
#include "runWarmUp.h"

using namespace std;
using namespace Rcpp;

//' @title runModel
//' @description run whole model including model initializing, warm-up period, water balance and routing and returns list with states and fluxes 
//' @param SimPeriod Period to simulate (usually defines as model[["SimPeriod"]] where model is object returned from basin.prepareRun())
//' @param ListConst list with all required information regarding basin and input (usually, object returned by basin.prepareRun())
//' @param Settings vector that is used to define settings
//' @param nYears number of years defined as warm-up (first year is then simulated n times, before starting with the actual simulation)
//' @export
// [[Rcpp::export]]
List runModel(DateVector SimPeriod, List ListConst, NumericVector Settings, int nYears){
	
	defSettings(Settings); //defines Settings
	initModel(ListConst); // defines Variables and Input data
	initializeModel(); // initializes Vectors that defines fluxes and states in Model
	
	if ((useSystemVals == 1) || (useSystemVals == 3)){
		setStorages(SimPeriod); //initial values will be read into the system
	}
	
	if ( ((useSystemVals == 1) || (useSystemVals == 3)) & (nYears > 0)) {
		stop("'nyears' should be equal to 0 when using using SystemValues to define initial storages!");
	}
	
	runWarmUp(SimPeriod, nYears); // simulating the first year nTimes to define fluxes and states in Model
	
	List WaterBalanceOutput = createWaterBalance(SimPeriod); //calculates WaterBalance
	
	List Fluxes = as<List>(WaterBalanceOutput["Fluxes"]);
		NumericMatrix surfaceRunoff = as<NumericMatrix>(Fluxes["dailyLocalSWRunoff"]);
		NumericMatrix GroundwaterRunoff = as<NumericMatrix>(Fluxes["dailyLocalGWRunoff"]);
		NumericMatrix PETw = as<NumericMatrix>(Fluxes["PETw"]);
		
	
	List RoutingOutput = routing(SimPeriod, surfaceRunoff, GroundwaterRunoff, PETw, Prec); // is quite slow - mm/day - excecutes routing
	List L = List::create(Named("daily") = WaterBalanceOutput, Named("routing") = RoutingOutput);
	
	if ((useSystemVals == 2) || (useSystemVals == 3)){
		writeStorages(SimPeriod); //system values will be write out
	}
	
	return(L);
}
