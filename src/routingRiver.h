#ifndef ROUTINGRIVER_H
#define ROUTINGRIVER_H

double routingRiver(int cell, double riverVelocity, double RiverInflow,
					NumericVector G_riverOutflow, NumericVector S_river);

double getRiverVelocity(int Type, int cell, double inflow);

void setLakeWetlandToMaximum(NumericVector S_locLakeStorage, NumericVector S_locWetlandStorage,
							NumericVector S_gloLakeStorage, NumericVector S_ResStorage,
							NumericVector S_gloWetlandStorage);

double estimate_bottom_width(double bankfull_flow_in_cell);
double estimate_bankfullflow_width(double bankfull_flow_in_cell);
#endif

