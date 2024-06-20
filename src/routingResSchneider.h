#ifndef ROUTINGRESSCHNEIDER
#define ROUTINGRESSCHNEIDER

#include <Rcpp.h>


using namespace std;
using namespace Rcpp;


double routingResSchneider(int day,
	int cell,
	Date SimDate,
	double PETWater,
	double PrecWater,
	double inflow,
	NumericVector Res_outflow,
	NumericVector Res_overflow,
	NumericVector Res_target,
	NumericVector S_ResStorage,
	NumericVector Res_evapo,
	NumericVector Res_inflow,
	NumericMatrix dailyUse,
	NumericVector MeanDemand,
	NumericVector Prec_forecast,
	NumericVector PET_forecast,
	NumericVector inflow_forecast,
	double &Res_storage_target,
	double &accumulated_daily_month_inflow);

long optimiseDamOperation(
	int res_type,
	int yearDate,
	double mean_inflow,
	double Smax,
	double Qmin_7day,
	double Qmax_7day,
	double Qbf,
	double Qflood,
	double Sstart,
	double *inflow_forecast,
	int month_current,
	double *PET_forecast,
	double *Prec_forecast,
	bool eFlow);

							

#endif