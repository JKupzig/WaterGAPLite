#ifndef ROUTINGRESHANASAKI_H
#define ROUTINGRESHANASAKI_H

#include <Rcpp.h>

using namespace std;
using namespace Rcpp;


double routingResHanasaki(int day, int cell, Date SimDate, double PETWater, double PrecWater, double inflow, 
							NumericVector Res_outflow, NumericVector Res_overflow, NumericVector S_ResStorage, NumericVector Res_evapo, NumericVector Res_inflow,
							NumericMatrix dailyUse, NumericVector MeanDemand);
							
int numberOfDaysInMonth(int month, int year);
int numberOfDaysInYear(int year);
 
#endif

