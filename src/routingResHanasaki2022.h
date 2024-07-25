#ifndef ROUTINGRESHANASAKI2022
#define ROUTINGRESHANASAKI2022

#include <Rcpp.h>


using namespace std;
using namespace Rcpp;


double routingResHanasaki2022(int day,
	int cell,
	Date SimDate,
	double PETWater,
	double PrecWater,
	double inflow,
	NumericVector Res_outflow,
	NumericVector Res_overflow,
	NumericVector S_ResStorage,
	NumericVector Res_evapo,
	NumericVector Res_inflow,
	double drainage_area);

#endif

