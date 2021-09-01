#ifndef ROUTINGGLOBALLAKES_H
#define ROUTINGGLOBALLAKES_H

double routingGlobalLakes(int cell, double PrecWater, double PETWater, double inflow,
			NumericVector gloLake_overflow, NumericVector gloLake_outflow , NumericVector S_gloLakeStorage, 
			NumericVector gloLake_evapo, NumericVector gloLake_inflow);
 
#endif

