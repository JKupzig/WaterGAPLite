#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;
using namespace std;

//' @title tools_DefDrainageCells
//' @description rcpp tool to define Drainage Cells 
//' @param Outlet of basin as GCRC number in continental grid
//' @param GCRC as vector for continent
//' @param OutflowMatrix as vector for continent 
//' @return IntegerVector with GCRC-IDs that are part of basin
//' @export
// [[Rcpp::export(rng = false)]]
IntegerVector tools_DefDrainageCells(int Outlet, IntegerVector GCRC, IntegerVector OutflowMatrix) {
    //function to define basin    
    int ng = OutflowMatrix.length();
    
    IntegerVector Outlet_hilf(ng);
    Outlet_hilf[0] = Outlet;
    
	
    int count1 = 1;
    IntegerVector result(ng);
    result[0] = Outlet;
    
    int first = 1;
	int loop_it = 1;
	int len;
	int count2 = 0;
		
	while (loop_it == 1) {
		if (first == 1){
		  len = 1;
		} else {
		  len = count2;
		}
		
		count2 = 0;
		first = 0;
		
		IntegerVector Outlet_hilf_c(ng);
		for (int i = 0; i < len; i++){ //looping through length of Outflow_hilf --> "outlets" for actual step
		  
		  for (int j = 0; j < ng; j++) {
			  
			  if(OutflowMatrix[j] == Outlet_hilf[i]) { // looping through outflow to get gcrc-ID for outlet
				result[count1] = GCRC[j];
				
				Outlet_hilf_c[count2] = GCRC[j]; //append entry on Vector
				count1 += 1;
				count2 += 1;
			  }
			}
		}
		Outlet_hilf = clone(Outlet_hilf_c);
		if (count2 == 0){
			loop_it = 0;
		}
	}

	return head(result, count1);
}


/* // [[Rcpp::export]]
NumericMatrix tools_getIntStorage(NumericVector LAI_min, NumericVector LAI_max, IntegerVector initDays, 
								  const NumericMatrix Temp, const NumericMatrix Prec, const NumericVector aridType, const NumericVector GLCT){
  //Ths function uses the model function as it is implemented iin WG3.1
  
   //info from extern --> global environment in R
  //const NumericMatrix Temp = Environment::global_env()["temp"]; 
  //const NumericMatrix Prec = Environment::global_env()["prec"];
  
  //const NumericVector aridType = Environment::global_env()["G_ARID_HUMID"];
  //const NumericVector GLCT = Environment::global_env()["GLCT"];  
  
  int nrow = Temp.nrow(); 
  int ncol = Temp.ncol(); 
    
  NumericMatrix dailyLAI (nrow, ncol);
  IntegerMatrix growingSeason (nrow, ncol);
  
  const double Tgrenz = 8.0; //as in model lai.cpp ll. 169
  const double PrecSumgrenz = 40;
	
	//this is the original model code implemented, however the output seems to be quite weird..
  for (int col = 0; col < ncol; col++){ //iterating through cells
	//for each cell everything is set to zero at the beginning of evaluation
	double PrecSum = 0;
	int GrowingStatus = 0;
	int days_since_start = 0;
	
	//iterating through days 
	for (int row = 0; row < nrow; row++){ 
		if (Temp(row, col) > Tgrenz){ //case 1
			if (GrowingStatus == 0) { // vegetation is not grown yet...
				if (days_since_start >= initDays[col]){ // ...but initial days of growing season have been reached
					days_since_start++;
					PrecSum += Prec(row,col);
					if(PrecSum > PrecSumgrenz ){ // threshold of min precipitation has been met and growing season can start
						if (days_since_start >= initDays[col] + 30){ // full vegetation development has been reached and status will switch
							days_since_start = initDays[col] + 30;
							GrowingStatus = 1;
						}
						// return either the LAI in sprout or the max LAI-value
						dailyLAI(row,col) = (LAI_min[col] + (LAI_max[col] - LAI_min[col]) * (days_since_start - initDays[col]) / 30.);
					} else { // sum of precipitation is not high enough to start growing season
						days_since_start = initDays[col];
						dailyLAI(row,col) = (LAI_min[col]);
					}
				} else { // initial days of growing season have not been reached yet
					days_since_start++;
					PrecSum += Prec(row,col);
					dailyLAI(row,col) = (LAI_min[col]);
				}
			} else{// vegetation is already fully grown...
				if (days_since_start <= 30) { // ...and we are in phase of senescence
					days_since_start--;
					if (GLCT[col] <= 2){ // if land_cover_type is '1' or '2' we have ervergreen plants and LAI will never clompletely degrade JK: not sure why this is done because LAImin is quite high for these plants
						GrowingStatus = 0; // therefore status will switch at once
					}
					if (days_since_start <= 0){ // LAI is completely degraded to min LAI and status will switch
						days_since_start = 0;
						GrowingStatus = 0;
						PrecSum = 0.;
					}
					dailyLAI(row,col) = (LAI_max[col] - (LAI_max[col] - LAI_min[col]) * (30 - days_since_start) / 30.);
				} else { // initial days for senescence phase have not been reached yet and we have growing conditions (again)
					if ( (aridType[col] != 1) & (Prec(row,col)<0.5) ){ // in arid regions we have no growing conditions if there is no rain 
						days_since_start--;
					} else {// if conditions for LAI reduction should happen day after day, use this  equation (reset for initial days)
						days_since_start = 30 + initDays[col];
					}
					dailyLAI(row,col) = (LAI_max[col]);
				}
			}
		} else { //case 2
			if (GrowingStatus == 0) { // vegetation is not fully grown yet 
				if (days_since_start > initDays[col]){ // initial days of growing season have been reached and plants will grow anyway
					(days_since_start)++;
					(PrecSum) += Prec(row,col);
					if(PrecSum > PrecSumgrenz){ // threshold of min precipitation has been met and growing season can start
						if (days_since_start >= initDays[col] + 30){
							days_since_start = initDays[col] + 30;
							GrowingStatus = 1;
						}
						dailyLAI(row,col) = (LAI_min[col] + (LAI_max[col] - LAI_min[col]) * (days_since_start - initDays[col]) / 30.);
					} else { // sum of precipitation is not high enough to start growing season
						days_since_start = initDays[col];
						dailyLAI(row,col) = (LAI_min[col]);
					}
				} else { // no growing season
					(PrecSum) += Prec(row,col);
					dailyLAI(row,col) = (LAI_min[col]);
				}
			} else { // we are in growing season but lai will be reduced now 
				if (days_since_start <= 30) {
					(days_since_start)--;
					if (days_since_start <= 0){
						days_since_start = 0;
						GrowingStatus = 0;
						PrecSum = 0.;
					}
					dailyLAI(row,col) = (LAI_max[col] - (LAI_max[col] - LAI_min[col]) * (30 - days_since_start) / 30.);
				} else { // we are in growing season but the inital days for lai degrading have not been met 
					(days_since_start)--;
					dailyLAI(row,col) = (LAI_max[col]);
				}
			}
		}
	}
  }
  return(dailyLAI);
}


// [[Rcpp::export]] 
NumericVector tools_InterpolateValues(NumericVector VectorIn, NumericVector Table_x, NumericVector Table_y) {
    //function to interpolate in table - not used at the moment  
    int cells = VectorIn.length();

    int len = Table_x.length();
    int count;
    double lowerLimit;
    double upperLimit;
    
    double val;
    NumericVector VectorOut (cells);
    
    double minVal = Table_x[0];
    double maxVal = Table_x[len-1];
    
    for (int cell = 0; cell < cells; cell++) {

	  val = VectorIn[cell];
	  if (val < minVal){
		VectorOut[cell] = -999.0;
	  } else if (val > maxVal) {
		VectorOut[cell] = -999.0;
	  } else {
		//Finding vales from table to interpolate
		lowerLimit = minVal;
		count = 0;
		while (lowerLimit < val) {
		   count +=1;
		   lowerLimit = Table_x[count];
		}
		
		if (lowerLimit != val){
			count -= 1;
		}
		lowerLimit = Table_x[count];
		
		if (lowerLimit == maxVal){
		  VectorOut[cell] = Table_y[len-1];
		} else if (val == minVal){
		  VectorOut[cell] = Table_y[0];
		} else {
		  upperLimit = Table_x[count+1];
		  //Interpolation formula
		  VectorOut[cell] = (val - lowerLimit) / (upperLimit - lowerLimit) * (Table_y[count+1] - Table_y[count]) + Table_y[count] ;
	}}} 
    return(VectorOut);
}
 */

//   if (growingSeason[i-1] == 0){ // check growing season of day before:crop is growing
//      
//      PrecSum = PrecSum + Prec[i];
//      if (Temp[i] > Tgrenz) {
//        count ++; }
//      if ((count >= initDays) & (PrecSum >= PrecSumgrenz)){
//        growingSeason[i] = 1;
//        count = initDays;
//      }
//    } else if (growingSeason[i-1] == 1){ // check growing season of day before: crop is degrading
//      if (Temp[i] <= Tgrenz) {
//        count --; }
//      if (count <= 0) {
//        growingSeason[i] = 0;
//        count = 0;
//        PrecSum = 0.;
//
//      }}