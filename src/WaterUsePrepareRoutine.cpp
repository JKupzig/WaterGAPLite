#include <Rcpp.h>
#include <math.h>
#include "ModelTools.h"
#include "initModel.h"
#include "WaterUsePrepareRoutine.h"

//' @title WaterUseCalcMeanDemandDaily
//' @description Prepare infromation for reservoir (calculate yearly mean demand of cell itself and next 20 downstream cells)
//' @param year year of simulation period as integer
//' @param GapYearType Info from Setting wheter 29.02 is simulated (0) or not (1)
//' @return G_mean_demand as numericVector for the sepcified year in [mm*km²/day]
//' @export
// [[Rcpp::export]]
NumericVector WaterUseCalcMeanDemandDaily(int year, int GapYearType){
	// info is used in reservoir
	NumericVector G_mean_demand(array_size); //longterm water demand of cell itself
	
	//calculate MEAN demand of downstream area
	for (int cell = 0; cell < array_size; cell++){
		
		G_mean_demand[cell] = YearlyMeanDemand[cell];
		
		int i=0; 
		int downstreamCell=outflowOrder[cell];
		
		while (i < reservoir_dsc && downstreamCell > 0 && downstreamCell < array_size && G_RESAREA[downstreamCell-1] == 0) {
			//suggestion Jenny: only consider positive values here
			G_mean_demand[cell] += YearlyMeanDemand[downstreamCell-1] * G_ALLOC_COEFF(i++, cell);
			// next downstream cell
			downstreamCell = outflowOrder[downstreamCell-1];
		}
	}
	
	//unit changing from m³/yr to mm*km²/day (to m³/s) --> /= 31536000.
	for (int cell = 0; cell < array_size; cell++){
		if (GapYearType == 1) { //avoid somulation of the 29.02 to compare modelling result to WG3
			G_mean_demand[cell] = G_mean_demand[cell] / 1000. *365.; //[mm*km²/day]
		} else {
			G_mean_demand[cell] = G_mean_demand[cell] / 1000. * numberOfDaysInYear(year); //[mm*km²/day]
		}
	}
	
	return(G_mean_demand);
}


//' @title WaterUseCalcDaily
//' @description calculates daily water use for groundwater and surface water (note that GapYearType needs to be included in function input)
//' @param waterUseType 0 (no water use), 1 (only water use without Transport to cities) or 2 (only water use with Transport to cities)
//' @param dailyUse Matrix with two rows that gives water use for actual day in mm*km²/day (first = GW, second=SW+TF), note that all days in one month in one year have same values
//' @param year year of simulation period as integer
//' @param month month of simulation period as integer
//' @param StartYear information to get the right column from read water use information
//' @param Info_GW read water use information from groundwater
//' @param Info_SW read water use information from surface water
//' @param Info_TF read water use information for transport to cities
//' @export
// [[Rcpp::export]]
void WaterUseCalcDaily(int waterUseType, NumericMatrix dailyUse, int year, int month, int StartYear, 
						NumericMatrix Info_GW, NumericMatrix Info_SW, NumericMatrix Info_TF){
	
	NumericVector GW_day (array_size);
	NumericVector SW_day (array_size);
	NumericVector TF_day (array_size);	
	
	
	//Note that with Lists is more flexible because SimPeriod can change and it still can be calculated withput the need of reading everythin in again
	int index = year-StartYear + month - 1; //Matrix for SW and GW starts by StartYear and January (monthly Data)
	int indexTF = year-StartYear; //Matrix for TF starts by StartYear (annual data)

	GW_day = Info_GW(index, _ );
	SW_day = Info_SW(index, _ );
	TF_day = Info_TF(indexTF, _ );
	
	int nYears = numberOfDaysInYear(year);
	int nMonths = numberOfDaysInMonth(month, year);
	
	if (GapYearType == 1) {
		nYears = 365;
		if (month == 2){ 
			nMonths = 28; 
		}
	}

	switch(waterUseType) {
		
		//no water use is considered
		case 0: GW_day.fill(0); 
				dailyUse(0,_) = GW_day;
				dailyUse(1,_) = GW_day;
				
		// only water use without Transport to cities is considered
		case 1: for (int i=0; i < array_size; i ++){
					// changing values from m³/year or month to mm*km²/day
					GW_day[i] = GW_day[i] / 1000 / nMonths;
					SW_day[i] = SW_day[i] / 1000 / nMonths;
					// adding domestic transfer to surface water net abstraciton
					dailyUse(0,i) = GW_day[i];
					dailyUse(1,i) = SW_day[i];
		}
		// water use including Transport to cities is considered
		case 2: for (int i=0; i < array_size; i ++){
					// changing values from m³/year or month to mm*km²/day
					TF_day[i] = TF_day[i] / 1000 / nYears;
					GW_day[i] = GW_day[i] / 1000 / nMonths;
					SW_day[i] = SW_day[i] / 1000 / nMonths;
					// adding domestic transfer to surface water net abstraciton
					dailyUse(0,i) = GW_day[i];
					dailyUse(1,i) = SW_day[i] + TF_day[i];
		}
	
	}
	
}