#include <Rcpp.h>
#include <math.h>
#include "initModel.h"
#include "WaterUseConsumSW.h"



//whin geht return flow. d.h. wenn water use hat negatives VZ
// geht immer in river!

// double AbstractFromCell(int cell, double remainingUse, NumericVector G_actualUse,
// 						NumericVector S_river, NumericVector S_ResStorage, NumericVector S_gloLakeStorage,
// 						NumericVector S_locLakeStorage);



//' @title AbstractFromCell
//' @description function that abstracts water use from storages 
//' @param cell cell in basin that is used for abstraction
//' @param remainingUse remainingUse of cell that needs to be satisfied with storages
//' @param G_actualUse actual use in cell in mm*km²/day
//' @param S_river river storage to satisfy uses (1)
//' @param S_ResStorage reservoir storage to satisfy uses (2)
//' @param S_gloLakeStorage global lake storage to satisfy uses (3)
//' @param S_locLakeStorage local lake storage to satisfy uses (4)
//' @return remainingUse after intention to satisfy uses with water storages mm
//' @export
double AbstractFromCell(int cell, double remainingUse, NumericVector G_actualUse,
						NumericVector S_river, NumericVector S_ResStorage, NumericVector S_gloLakeStorage,
						NumericVector S_locLakeStorage) {
	
	int StartVal = remainingUse;
	// first step: water is taken out of the river (or returned to river!)
	if (remainingUse < S_river[cell]) {
		S_river[cell] -= remainingUse;
		remainingUse = 0.;
	} else {
		remainingUse -= S_river[cell];
		S_river[cell] = 0.;
	}
	
	
	// second step: take water out of reservoirs!
	if (remainingUse > 0) {
		if ((G_RESAREA[cell] > 0) && (S_ResStorage[cell] > (G_STORAGE_CAPACITY[cell] * 0.1))) {
			// storage volume of the reservoir has to be more than 10% of capacity
			// otherwise no water is taken out of the reservoir
			// do not allow water use below the 10% level!
			if (remainingUse < (S_ResStorage[cell] - (G_STORAGE_CAPACITY[cell] * 0.1))) {
				S_ResStorage[cell] -= remainingUse;
				remainingUse = 0;
			} else {
				remainingUse -= (S_ResStorage[cell] - (G_STORAGE_CAPACITY[cell] * 0.1));
				S_ResStorage[cell] = G_STORAGE_CAPACITY[cell] * 0.1;
			}
		}
	}
	
	
	// third step: take water from global lakes
	if (remainingUse > 0) {
		if (((G_LAKAREA[cell]) > 0) && (S_gloLakeStorage[cell] > 0)) {
			// water level of the lake has to be above 0 m
			// otherwise no water is taken out of the global lake
			if (remainingUse < S_gloLakeStorage[cell]) {
				S_gloLakeStorage[cell] -= remainingUse;
				remainingUse = 0;
			} else {
				remainingUse -= S_gloLakeStorage[cell];
				S_gloLakeStorage[cell] = 0;
			}
		}
	}

	// fourth step: take water from local lakes
	if (remainingUse > 0) {
		if ((G_LOCLAK[cell] > 0)	&& (S_locLakeStorage[cell] > 0)) {
			if (remainingUse < S_locLakeStorage[cell]) {
				S_locLakeStorage[cell] -= remainingUse;
				remainingUse = 0;
			} else {
				remainingUse -= S_locLakeStorage[cell];
				S_locLakeStorage[cell] = 0;
			}
		}
	}
	
	G_actualUse[cell] += (StartVal - remainingUse);
	
	return(remainingUse);
}


//' @title SubtractWaterConsumSW
//' @description function that distributes water use spatial and/or temporal, needs helper function AbstractFromCell for water use abstraction
//' @param WaterUseAllocationType 0 (spatial and temporal distribution), 1 (spatial distribution), 2 (temporal distribution) 
//' @param dailyUse Matrix with two rows that gives water use for actual day in mm*km²/day (first = GW, second=SW+TF), note that all days in one month in one year have same values
//' @param G_totalUnsatisfiedUse unsatisfied uses that are potentially spatial and/or temporal distributed
//' @param S_river river storage to satisfy uses (1)
//' @param S_ResStorage reservoir storage to satisfy uses (2)
//' @param S_gloLakeStorage global lake storage to satisfy uses (3)
//' @param S_locLakeStorage local lake storage to satisfy uses (4)
//' @param G_actualUse actual use in cell in mm*km²/day
//' @export
void SubtractWaterConsumSW(int WaterUseAllocationType, NumericMatrix dailyUse, NumericVector G_totalUnsatisfiedUse,
						   NumericVector S_river, NumericVector S_ResStorage, NumericVector S_gloLakeStorage, 
						   NumericVector S_locLakeStorage, NumericVector G_actualUse) {
	
	
	double dailyUseSW=0;
	double totalDesiredUse=0;
	double remainingUse=0;
	
	for (int cell = 0; cell < array_size; cell++) {

		dailyUseSW= dailyUse.at(1,cell);	//  mm*km²/day
		
		
		// for TEMPORAL distribution
		if (WaterUseAllocationType != 1) {
			//G_totalUnsatisfiedUse [mm*km²/day] contains unsatisfied use of earlier time steps
			totalDesiredUse = dailyUseSW + G_totalUnsatisfiedUse[cell];
		} else {
			// to avoid TEMPORAL distribution --> 1: only spatial distribution is considered
			totalDesiredUse = dailyUseSW;
		}
		
		// 'remainingUse' contains always the amount of water that has not been satisfied
		remainingUse = totalDesiredUse;
		
		remainingUse = AbstractFromCell(cell, remainingUse, G_actualUse,
										S_river, S_ResStorage, S_gloLakeStorage,
										S_locLakeStorage);

		// satisfied use of the current day summed up for the whole year
		// G_satisfiedUse[n] += totalDesiredUse - remainingUse;
		// for new Use allocation (M.Hunger 2/2006)
		// all actual satisfied water uses
		//G_actualUse[cell] += totalDesiredUse - remainingUse;
		G_totalUnsatisfiedUse[cell] = remainingUse;
		
	}
	
	
	// now it is looked up in the neighbouringcells (to account for inaccuracy in modeling) and the next 20 downstream cells to satisfy demand
	// for SPATIALLY distribution
	if (WaterUseAllocationType != 2) {
		
		short i;
		int index;
		int secondCell;
		int downstreamCell;
		double storageSum;
		double totalNeighbourStorage;
		double totalRemainingUse;
		

		for (int cell = 0; cell < array_size; cell++) {

			totalRemainingUse = G_totalUnsatisfiedUse[cell]; // for new Use allocation (M.Hunger 2/2006)
			totalNeighbourStorage = 0;
			secondCell = -1;
			
			//finding neighbouring station from cell (within basin) with largest storage volume 
			for (i = 0; i < 8; i++) {
				index = (NeighbouringCells(i, cell) -1) ;
				if (index == 0){
					storageSum = 0; //than no neighbouring cell for this position
				} else {
					storageSum = S_river[index]
								+ S_locLakeStorage[index]
								+ S_gloLakeStorage[index]
								+ S_ResStorage[index];
				} 
				
				if (storageSum > totalNeighbourStorage) {
					totalNeighbourStorage = storageSum;
					secondCell = index;
				}
			}
			
			//calculate abstraction from neighbouring cell first 
			if (secondCell >= 0) {
				totalRemainingUse = AbstractFromCell(cell, remainingUse, G_actualUse,
										S_river, S_ResStorage, S_gloLakeStorage,
										S_locLakeStorage);
			}
			
			// if water demand is still not satisfied, abstract water from next 20 downstream stations
			i = 0;
			downstreamCell=outflowOrder[cell];
			while (totalRemainingUse > 0 && i < reservoir_dsc && downstreamCell >= 0){
				
				if (downstreamCell != secondCell){
					totalRemainingUse = AbstractFromCell(cell, remainingUse, G_actualUse,
										S_river, S_ResStorage, S_gloLakeStorage,
										S_locLakeStorage);
				}
				
				downstreamCell = outflowOrder[downstreamCell-1];
				i++;
			}
			
			G_totalUnsatisfiedUse[cell] = totalRemainingUse; //iis set to 0 for 01.01.XXXX
		}
	}
		
		
} 

