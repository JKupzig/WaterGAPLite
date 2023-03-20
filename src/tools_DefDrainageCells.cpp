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
