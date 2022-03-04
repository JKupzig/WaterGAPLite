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
//' @param Settings vector of length 8 that is used to define settings:
//'  \itemize{
//'   \item 1st entry: water use               -> 0 (off), 1 (on), 2 (on, including water transport to cities)
//'   \item 2nd entry: water use allocation    -> 0 (temporal & spatial distr.), 1 (spatial distr.), 2 (temporal distr.)
//'   \item 3rd entry: flow velocity           -> 0 (constant), 1 (variable)
//'   \item 4th entry: gap year                -> 0 (including 29.02), 1 (without 29.02)
//'   \item 5th entry: reservoir algorithm     -> 0 (Hanasaki), 1 (global lake)
//'   \item 6th entry: splitting factor        -> 0 (original version), 1 (set as parameter) - only used for development purposes
//'   \item 7th entry: long wave radiation     -> 0 (reading), 1 (calculating)
//'   \item 8th entry: warm up period          -> 0 (no system values), 1 (system values are read), 2 (system values are written), 3 (system values are read and written)
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
