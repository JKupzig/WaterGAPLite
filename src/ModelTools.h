
#ifndef MODELTOOLS_H
#define MODELTOOLS_H

#include <Rcpp.h>

using namespace std;
using namespace Rcpp;

IntegerVector findNumberInVector(int number, IntegerVector vec);
IntegerVector findUniqueValues(IntegerVector vec);
IntegerVector sortIt(IntegerVector vec);
double sumVector(NumericVector vec);
int numberOfDaysInMonth(int month, int year);
int numberOfDaysInYear(int year);

#endif