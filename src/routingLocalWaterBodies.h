#ifndef ROUTINGLOCALWATERBODIES_H
#define ROUTINGLOCALWATERBODIES_H

double routingLocalWaterBodies(bool Type, int cell, double PrecWater,  double PETWater, double TempWater, NumericVector dday, NumericVector SnowStorageWetland, double Inflow,
								NumericVector S_locLakeStorage, NumericVector locLake_overflow, NumericVector locLake_outflow, NumericVector locLake_evapo, NumericVector locLake_inflow,
								NumericVector S_locWetlandStorage, NumericVector locWetland_overflow, NumericVector locWetland_outflow, NumericVector locWetland_evapo, NumericVector locWetland_inflow);
								
								#endif