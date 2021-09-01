#include <Rcpp.h>

using namespace std;
using namespace Rcpp;

#ifndef DAILY_H
#define DAILY_H

List createWaterBalance(DateVector timestring);

#endif