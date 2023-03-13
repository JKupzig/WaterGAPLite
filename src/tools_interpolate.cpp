#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;
using namespace std;

//' @title tools_interpolate
//' @description rcpp function for interpolation of a vector (alternative could be using r function and apply)
//' @param VectorIn vector as base for interpolation
//' @param Table_x Table_x of values
//' @param Table_y Table_y of values
//' @return NumericVector with interpolated output
//' @export
// [[Rcpp::export(rng = false)]]
NumericVector tools_interpolate(NumericVector VectorIn, NumericVector Table_x, NumericVector Table_y) {
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
