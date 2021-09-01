#ifndef WATERUSEPREPAREROUTINE_H
#define WATERUSEPREPAREROUTINE_H

#include <Rcpp.h>

using namespace std;
using namespace Rcpp;


NumericVector WaterUseCalcMeanDemandDaily(int year, int GapYearType);
void WaterUseCalcDaily(int waterUseType, NumericMatrix dailyUse, int year, int month, int StartYear, NumericMatrix Info_GW, NumericMatrix Info_SW, NumericMatrix Info_TF);
 
#endif