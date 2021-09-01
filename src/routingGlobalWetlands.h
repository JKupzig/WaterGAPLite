#ifndef ROUTINGGLOBALWETLANDS_H
#define ROUTINGGLOBALWETLANDS_H

double routingGlobalWetlands(int cell, double PrecWater, double PETWater, double inflow,
		NumericVector gloWetland_overflow, NumericVector gloWetland_outflow, NumericVector S_gloWetlandStorage, 
		NumericVector gloWetland_evapo, NumericVector gloWetland_inflow);
 
#endif

