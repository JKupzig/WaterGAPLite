#include <Rcpp.h>
#include <math.h>
#include "ModelTools.h"
#include "initModel.h"
#include "routingResHanasaki2022.h"

using namespace Rcpp;
using namespace std;

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
                              NumericVector Res_inflow)
{
    int dayDate = SimDate.getDay();     // day that is simulated
    int monthDate = SimDate.getMonth(); // month that is simulated
    int yearDate = SimDate.getYear();   // year that is simulated
    int februaryDays = 28;
	if (GapYearType != 1)
	{
		februaryDays = numberOfDaysInMonth(2, yearDate);
	}
	int daysInMonthArray[12] = {31, februaryDays, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
    int daysInMonth = daysInMonthArray[monthDate - 1];

    double totalInflow; 
	double outflow;
	double overflow;
	double evaporation;
	double gloResEvapoReductionFactor;

    double Vtotal = G_STORAGE_CAPACITY[cell] * 1000. * 1000.; // [km3] => [mm*km²]
    double Vflood = Vtotal * 0.7;
    double Vemergency = Vflood + 0.2 * (Vtotal - Vflood);
    double Vcontrol = Vflood / 2;

    double Qflood = G_BANKFULL[cell] / 1000. * 3600. * 24.; // [m3/s] => [mm*km²/day]
    double Qnormal = G_MEAN_INFLOW[cell] * 1000. * 1000. / daysInMonth; // [km3/month] => [mm*km²/day]

    double drainage_area = 0;
    

    double k = max(1 - 5 * (Vtotal - Vflood)/drainage_area, 0.);

    totalInflow = inflow + ( PrecWater * G_RESAREA[cell]);//[mm km²]
    S_ResStorage[cell] += totalInflow;
    if (S_ResStorage[cell] > G_STORAGE_CAPACITY[cell])
		gloResEvapoReductionFactor = 1.;
	else
		gloResEvapoReductionFactor = 1. - pow(fabs(S_ResStorage[cell] - Vtotal)/ Vtotal, evapoReductionExpReservoir);

	// calculate evaporation from global lakes
	evaporation = (PETWater * gloResEvapoReductionFactor)* G_RESAREA[cell]; // [mm km²]

	// substract global lake evapo
	S_ResStorage[cell] -= evaporation; //[mm km²]

	// if G_gloResStorage is below '0' storage and surfStorageEvapo has to be adjusted
	if (S_ResStorage[cell] < 0.){
		evaporation += S_ResStorage[cell];// [mm km²]
		S_ResStorage[cell] = 0.;
	}
    // Algorithm
    if (totalInflow >= G_BANKFULL[cell]) // case A
    {
        if (S_ResStorage[cell] <= Vcontrol){
            outflow = Qnormal * S_ResStorage[cell] / Vflood;
        } else if (S_ResStorage[cell] <= Vflood){
            outflow = Qnormal / 2 + (S_ResStorage[cell] - Vcontrol) / (Vflood - Vcontrol) * (Qflood - Qnormal);
        } else if (S_ResStorage[cell] <= Vemergency){
            outflow = Qflood + k * (S_ResStorage[cell] - Vflood) / (Vemergency - Vflood) * (totalInflow - Qflood);
        } else {
            outflow = Qflood + (S_ResStorage[cell] - Vemergency) / (Vtotal - Vemergency) * (Qflood - Qnormal);

        }
    } else // case B
    {

    }


    // end algo

	Res_outflow[cell] = outflow; 
    Res_evapo[cell] = evaporation; 
	Res_inflow[cell] = totalInflow; 

    return outflow;
};
