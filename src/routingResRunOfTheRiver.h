#ifndef ROUTINGRESRUNOFTHERIVER
#define ROUTINGRESRUNOFTHERIVER

#include <Rcpp.h>

using namespace std;
using namespace Rcpp;

double routingResRunOfTheRiver(int cell,
                               double PETWater,
                               double PrecWater,
                               double inflow,
                               NumericVector Res_outflow,
                               NumericVector Res_overflow,
                               NumericVector S_ResStorage,
                               NumericVector Res_evapo,
                               NumericVector Res_inflow);

#endif