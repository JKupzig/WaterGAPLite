#include <Rcpp.h>

using namespace std;
using namespace Rcpp;

#ifndef ROUTING_H
#define ROUTING_H

List routing(DateVector SimPeriod, NumericMatrix surfaceRunoff, NumericMatrix GroundwaterRunoff,
			NumericMatrix PETw, NumericMatrix Prec);

void CheckResType();

void setLakeWetlandToMaximum(
	NumericVector S_locLakeStorage,
	NumericVector S_locWetlandStorage,
	NumericVector S_gloLakeStorage,
	NumericVector S_ResStorage,
	NumericVector S_gloWetlandStorage);

#endif