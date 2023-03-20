#include <Rcpp.h>
#include <math.h>
#include "ModelTools.h"
#include "initModel.h"
#include "routingResHanasaki.h"


using namespace Rcpp;
using namespace std;


//' @title routingResHanasaki
//' @description function that defines routing through reservoir (after Hanasaki)
//' @param day of simulation period (0 = 1st day of simulation period)
//' @param cell cell that is simulated
//' @param SimDate Date of day which is simulated
//' @param PETWater Potential Evaporation from water [mm]
//' @param PrecWater Pecipitation above cell [mm]
//' @param inflow inflow to reservoir from network [mm*km²]
//' @param Res_outflow  outflow from reservoir (excluding overflow) [mm*km²]
//' @param Res_overflow overflow of reservoir [mm*km²]
//' @param S_ResStorage storage of reservoir [mm*km²]
//' @param Res_evapo evaporation from reservoir: evaporation = (PETWater * gloResEvapoReductionFactor)* G_RESAREA[cell] [mm*km²]
//' @param Res_inflow inflow to reservoir: inflow + ( PrecWater * G_RESAREA[cell]);//[mm km²]
//' @param dailyUse information of water that needs to be abstracted from surface water (second row of NumericMatrix)
//' @param MeanDemand output of WaterUseCalcMeanDemandDaily(year, GapYearType)
//' @return total outflow form reservoir: outflow + overflow [mm*km²]
//' @export
double routingResHanasaki(int day, int cell, Date SimDate, double PETWater, double PrecWater, double inflow, 
							NumericVector Res_outflow, NumericVector Res_overflow, NumericVector S_ResStorage, NumericVector Res_evapo, NumericVector Res_inflow,
							NumericMatrix dailyUse, NumericVector MeanDemand) {

	int dayDate = SimDate.getDay(); //day that is simulated
	int monthDate = SimDate.getMonth(); // month that is simulated
	int yearDate = SimDate.getYear(); // year that is simulated
	int daysInMonth = numberOfDaysInMonth(monthDate, yearDate);
	
	double maxStorage; 
	double totalInflow; 
	double outflow;
	double overflow;
	double evaporation;
	
	double gloResEvapoReductionFactor;  //KF *** for new reservoir algorithm
	double c_ratio;
	double prov_rel;      //provisional release
	double release;
	double dailyUseCell;
	
	// G_MEAN_INFLOW in km³/month --> mm*km²/day --> *1000 * 1000 / daysInMonth()
	double meanInflow = G_MEAN_INFLOW[cell]*1000*1000/daysInMonth; //[mm*km²/day]
	
	//NumericVector annual_release (array_size);
	NumericVector K_release (array_size);
		
		
	// define storage capacity to mean annual inflow ratio (c_ratio)
	// G_mean_inflow:  km³/month,  G_stor_cap:  km3/yr
	c_ratio = G_STORAGE_CAPACITY[cell]/(G_MEAN_INFLOW[cell]*12);
	maxStorage = G_STORAGE_CAPACITY[cell] * 0.85 * 1000 *1000; //85 % of storage capacity (from published volume data) [mm km²]
	
	// ######### CALCULATING WATERBALANCE ANALOG TO ALL WATERBODIES ###############
	
	//inflow from upstream PLUS lake water balance
	totalInflow = inflow + ( PrecWater * G_RESAREA[cell]);//[mm km²]
				
	// add inflow to storage
	S_ResStorage[cell] += totalInflow;

	//ET: same as gloLakeEvapo
	if (S_ResStorage[cell] > maxStorage)
		gloResEvapoReductionFactor = 1.;
	else
		gloResEvapoReductionFactor = 1. - pow(fabs(S_ResStorage[cell] - maxStorage)
							/ maxStorage, evapoReductionExpReservoir);

	// calculate evaporation from global lakes
	evaporation = (PETWater * gloResEvapoReductionFactor)* G_RESAREA[cell]; // [mm km²]

	// substract global lake evapo
	S_ResStorage[cell] -= evaporation; //[mm km²]

	// if G_gloResStorage is below '0' storage and surfStorageEvapo has to be adjusted
	if (S_ResStorage[cell] < 0.){
		evaporation += S_ResStorage[cell];// [mm km²]
		S_ResStorage[cell] = 0.;
	}
	
	// ######### APPLYING RESERVOIR ALGORITHM AFTER HANASAKI ###############
	
	//set rules at the beginning of the operational year! (only once per operational year!)
	if ((dayDate == 1) & (monthDate == G_START_MONTH[cell])){
		//calculate release coefficient for the actual year:
		//reduce release coefficent in this year to refill storage volume in reservoir
		if (S_ResStorage[cell] < (G_STORAGE_CAPACITY[cell]*1000*1000 * 0.1)) {
			K_release[cell] = 0.1;
		} else {
			K_release[cell] = S_ResStorage[cell] / (G_STORAGE_CAPACITY[cell]*1000*1000);
		}

	}
	
	// algorithm based on water use
	if (G_RES_TYPE[cell] == 1) {// (irrigation reservoir)

		//calculate monthly demand of downstream area
		dailyUseCell = dailyUse(1,cell); //Surface Water Net abstraction for cell in mm*km²/day

		// sum up water use of the next downstream cells of the reservoir within the next 20 routing steps considering allocation coeffcient
		// approach to estimate water demand that can be satisfied by reservoir
		// limits are: 1) 20 routing steps 2) no more donwstream cell (ocean/basin border) 3) another reservoir
		// at the moment basin outlet is defined with -999 in outflow, needs to be changed when simulating more than one basin!
		int i=0; 
		int downstreamCell=outflowOrder[cell];
		while (i < reservoir_dsc && downstreamCell > 0 && downstreamCell < array_size && G_RESAREA[downstreamCell-1] == 0) {
			//suggestion Jenny: only consider positive values here
			dailyUseCell += dailyUse(1,downstreamCell-1) * G_ALLOC_COEFF(i++, cell);
			// next downstream cell
			downstreamCell = outflowOrder[downstreamCell-1];
		}

				
		if(MeanDemand[cell] < 0 || dailyUseCell < 0 ){
			prov_rel = meanInflow;
		} else if (MeanDemand[cell] >= 0.5 * meanInflow){
			//provisional monthly release = i_mean /2. * (1. + monthly_demand_sum / d_mean_annual)
			prov_rel = meanInflow/2. * ( 1. + dailyUseCell/MeanDemand[cell]); // [mm*km²/day]
		} else {
			//provisional monthly release = i_mean + monthly_demand_sum - d_mean)
			prov_rel = meanInflow + dailyUseCell - MeanDemand[cell]; // [mm*km²/day]
		}
	} else if ((G_RES_TYPE[cell] >= 2) ) {//(non-irrigation: domestic & hydropower)  //$$$Christof: resOpt ==1 (Hanasaki)
		prov_rel = meanInflow; // [mm*km²/day]
	} else {
	  prov_rel = 0.;
	}

	// calculate release //[mm km²/day]
	if (c_ratio >= 0.5) {
		release = K_release[cell] * prov_rel;
	} else { // after Hanasaki (2006)--> 1/0,5^2=1/0,25=4
		release = ((4.*c_ratio*c_ratio) * K_release[cell] * prov_rel)
				+ (1.0 - (4.*c_ratio*c_ratio)) * inflow;
	}

	//new outflow
	//reservoir storage volume should not be less than 10% of maximum!)...
	if (S_ResStorage[cell] >= (G_STORAGE_CAPACITY[cell]*1000*1000 * 0.1)) {
		outflow = release;  //m3/s  ->  km3/routing time step
	} else { //...otherwise outflow will be reduced! (outflow should not be stopped, because we should serve ecosystem demands)
		outflow = 0.1 * release;
	}
	// substract outflow from storage
	S_ResStorage[cell] -= outflow;

	// reduce G_gloResStorage to maximum storage capacity -->when there is a lot of precipitation in this time step
	if (S_ResStorage[cell] > maxStorage) {
		overflow = (S_ResStorage[cell] - maxStorage);
		S_ResStorage[cell] = maxStorage;
	} else {
		overflow = 0;
	}
	// if G_gloResStorage is below '0' there is no outflow anymore
	if (S_ResStorage[cell] < 0.) {
		outflow += S_ResStorage[cell];
		S_ResStorage[cell] = 0.;
	}
	
	Res_outflow[cell] = outflow; 
	Res_overflow[cell] = overflow; 
	Res_evapo[cell] = evaporation; 
	Res_inflow[cell] = totalInflow; 
	
	return(outflow+overflow); // mm*km²
} 



