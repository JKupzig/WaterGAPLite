#ifndef WATERUSECONSUMESW_H
#define WATERUSECONSUMESW_H

void SubtractWaterConsumSW(int WaterUseAllocationType, NumericMatrix dailyUse, NumericVector G_totalUnsatisfiedUse,
						   NumericVector S_river, NumericVector S_ResStorage, NumericVector S_gloLakeStorage, 
						   NumericVector S_locLakeStorage, NumericVector G_actualUse);
 
#endif