#include <Rcpp.h>
#include <math.h>
#include "ModelTools.h"

using namespace Rcpp;
using namespace std;



// =============================================== HELP FUNCTION ===============================================================

//' @title findNumberInVector
//' @description Function that finds number in vector and return indices
//' @param number integer to search for
//' @param vec Integervector that is searched in 
//' @return indices of number in vector (0=1st entry of vector) 
//' @export
// [[Rcpp::export]]
IntegerVector findNumberInVector(int number, IntegerVector vec){
	
	const int ng = vec.length();
	IntegerVector result (ng);
	int count = 0;
	
	for (int i = 0; i < ng; i ++){
		if (vec[i] == number){
			result[count] = i;
			count ++;
		}
	}
	 return head(result, count);
}

//' @title findUniqueValues
//' @description Function that returns values in vector without duplicates
//' @param vec Integervector that is examined
//' @return Vector with values in vector without duplicates
//' @export
// [[Rcpp::export]]
IntegerVector findUniqueValues(IntegerVector vec){
	
	const int ng = vec.length();
	IntegerVector result (ng);
	int number;
	int count = 0;
	bool skip;
	
	//iterate through input vector 
	for (int i = 0; i < ng; i++){
		number = vec[i];
		skip = false;
		//look if number is already in result vector --> skip = true
		for (int j = 0; j < ng; j++){
			if (result[j] == number){
				skip = true;
				break;
			}
		}
		
		// if number is not in result yet skip is false and number is added to result
		if (skip != true) { 
			result[count] = number;
			count ++;
		}
	}
	return head(result, count);
}

//' @title sortIt
//' @description Function that sort a vector in ascending order (or descending not sure about that)
//' @param vec Integervector that is examined
//' @return sorted vector
//' @export
// [[Rcpp::export]]
IntegerVector sortIt(IntegerVector vec){
	IntegerVector vecCopy;
	vecCopy = clone(vec);
    std::sort(vecCopy.begin(), vecCopy.end());
    return vecCopy;
}

//' @title sumVector
//' @description Function that sums up a vector
//' @param vec Numericvector that is sumed up
//' @return sum of vector as double
//' @export
// [[Rcpp::export]]
double sumVector(NumericVector vec){ //should be written more generic but don't know how yet
	double SumVec = 0;
	for (int i = 0; i < vec.length(); i++){
		SumVec += vec[i];
	}
	return(SumVec);
}

//' @title numberOfDaysInMonth
//' @description Function that gives the number of Days in month
//' @param month as integer (1 = january)
//' @param year as integer 
//' @return number of days of specified month in specified year as integer
// [[Rcpp::export]]
int numberOfDaysInMonth(int month, int year){
	Date start = Date(month, 1, year); // 2000-01-02 Date(mon, day, year)
	int newMonth = month;
	int count = 0;
	while (newMonth == month){
		start = start + 1;
		newMonth = start.getMonth(); 
		count ++;
	}
	return(count);
}

//' @title numberOfDaysInYear
//' @description Function that gives the number of Days in year
//' @param year as integer 
//' @return number of days of specified year as integer
// [[Rcpp::export]]
int numberOfDaysInYear(int year){
	Date start = Date(1, 1, year); // 2000-01-02 Date(mon, day, year)
	int newYear = year;
	int count = 0;
	while (newYear == year){
		start = start + 1;
		newYear = start.getYear();
		count ++;
	}
	return(count);
}