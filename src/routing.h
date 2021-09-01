#include <Rcpp.h>

using namespace std;
using namespace Rcpp;

#ifndef ROUTING_H
#define ROUTING_H

List routing(DateVector SimPeriod, NumericMatrix surfaceRunoff, NumericMatrix GroundwaterRunoff, 
			NumericMatrix PETw, NumericMatrix Prec);
			
#endif