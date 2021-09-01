#include <Rcpp.h>
#include <math.h>
#include "initModel.h"
#include "dailyImmediateRunoff.h"

using namespace Rcpp;
using namespace std;

//' @title Calculate immediate run-off from sealed areas
//# @description { rcpp function to calculate immediate run-off based on  built-up fraction
//' over built-up areas 50 percent of precipitation is immediate runoff }
//' @param dailyEffPrec effective precipitation (throughfall of canopy + snowmelt water - fallen snow)
//' @param immediate_runoff run-off that contributed directly to the fast surface run-off in mm/d
//' @export
///////////////////////////////////////// immediate runoff //////////////////////////////////////////////////////////////////////////////


void dailyImmediateRunoff(NumericVector dailyEffPrec, NumericVector immediate_runoff){
	
	//const NumericVector GBUILTUP = Environment::global_env()["GBUILTUP"]; //amount of sealed ares in grid [-]
	//const NumericVector G_CORR_FACTOR = Environment::global_env()["G_CORR_FACTOR"]; //amount of sealed ares in grid [-]
	//const double runoffFracBuiltUp = Environment::global_env()["runoffFracBuiltUp"]; //0.5 [-]
	//const int cells = GBUILTUP.length(); 
	
	//these vectors are changing in this function
	//NumericVector immediate_runoff = Environment::global_env()["immediate_runoff"]; //amount of sealed ares in grid [-]
	//NumericVector dailyEffPrec = Environment::global_env()["dailyEffPrec"]; //amount of sealed ares in grid [-]
	
	for (int cell = 0; cell < array_size; cell++){
		immediate_runoff[cell] = runoffFracBuiltUp * dailyEffPrec[cell] * GBUILTUP[cell];
		dailyEffPrec[cell] -= immediate_runoff[cell];
		//immediate_runoff[cell] *= G_CORR_FACTOR[cell];
	}
	
	//List L = List::create(immediate_runoff);
	//return(immediate_runoff);
}