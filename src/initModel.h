

#ifndef INITMODEL_H
#define INITMODEL_H

#include <Rcpp.h>

using namespace std;
using namespace Rcpp;

//declaration of global constant variables that do not change in the whole simulation (static variables)
// this makes a shallow copy, with the R object underlying
// when the object do not change there is not much additional storage needed
// because the object are referred to as "const" they can't change in the whole script! --> I think everything is fine
// ATTENTION: redefinition of object name is possible, so when list is redefined than it is actually possible to change objects

extern int waterUseType;
extern int GapYearType;
extern int flowVelocityType;
extern int WaterUseAllocationType;
extern int ReservoirType;
extern int splitType;
extern int calcLong;
extern int useSystemVals;

extern void defSettings(NumericVector Settings);
extern NumericMatrix getLAIdaily(NumericVector LAI_min, NumericVector LAI_max, NumericVector initDays,
					    const NumericMatrix Temp, const NumericMatrix Prec, const IntegerVector aridType, const NumericVector GLCT);

extern String SystemValues;
extern int id;

extern NumericMatrix Temp;
extern NumericMatrix Rs;
extern NumericMatrix Rl;
extern NumericMatrix Prec; //WaterContent at the end of forstep
extern int cor_row;
extern NumericMatrix G_Elevation; //elevation of grid and subgrids
extern NumericMatrix NeighbouringCells; // neighbour cells where 0 indicates that there is no neighbour cell in the basin
// 4 3 2
// 5   1
// 6 7 8
extern IntegerVector GR;

extern NumericVector LAI_min;
extern NumericVector LAI_max;
extern NumericVector initDays;
extern NumericVector GLCT;
extern NumericMatrix dailyLaiAll;

extern NumericMatrix Info_GW;
extern NumericMatrix Info_SW;
extern NumericMatrix Info_TF;
extern NumericVector YearlyMeanDemand;

extern NumericVector albedo;
extern NumericVector albedoSnow;
extern NumericVector emissivity;
extern NumericVector alphaPT;
extern NumericVector degreeDayFactor;
extern NumericVector GBUILTUP;
extern NumericVector G_GAMMA_HBV; //calibrated gamma value
extern NumericVector maxDailyPET; ////precipitation + snow melt that comes to soil
extern NumericVector G_Smax ; //size of soil layer/storage
extern IntegerVector G_ARID_HUMID;
extern NumericVector G_TEXTURE;
extern NumericVector G_RG_max;
extern NumericVector G_gwFactor;
extern NumericVector GAREA;
extern NumericVector landfrac;
extern NumericVector G_ALLOC_COEFF;
extern IntegerVector G_LOCLAK; // % of cell that belongs to local lake
extern IntegerVector G_LOCWET; // % of cell that belongs to local wetland
extern IntegerVector G_GLOLAK; // % of cell that belongs to global lake (including resevroirs at the moment)
extern IntegerVector G_GLOWET; // % of cell that belongs to global wetland
extern NumericVector G_RESAREA; // km² reservoir area defined in outlet cell of reservoirs
extern NumericVector G_LAKAREA; // km² global lake area defined in outlet cell of global lake
extern NumericVector G_STORAGE_CAPACITY;
extern NumericVector G_MEAN_INFLOW;
extern NumericMatrix G_MEAN_INFLOW_MONTHLY;
extern IntegerVector G_START_MONTH;
extern IntegerVector G_RES_TYPE;
extern IntegerVector routeOrder;
extern IntegerVector outflowOrder; // obtained from routing input, modified

extern NumericVector G_BANKFULL; // BANKFULL flow in m³/s (is simulation product)
extern NumericVector G_riverLength;
extern NumericVector G_riverSlope;
extern NumericVector G_riverRoughness;

extern NumericVector G_7daymin;
extern NumericVector G_7daymax;
extern NumericVector flow_acc;

extern NumericVector Splitfactor;

extern double maxCanopyStoragePerLAI; // 0.3 mm
extern double canopyEvapoExp; // 0.6666667 [-]
extern int array_size; //
extern double snowFreezeTemp; // 0
extern double snowMeltTemp;
extern double runoffFracBuiltUp;
extern double  pcrit; // 12.5 mm/day
extern double  k_g;
extern double lakeDepth; // 0.005 km --> 5000 mm
extern double lakeOutflowExp; // 1.5 [-]
extern double wetlandDepth; // 0.002 km --> 2000 mm
extern double wetlOutflowExp; // 2.5 [-]
extern double evapoReductionExp; // 3.32193
extern double evapoReductionExpReservoir;
extern int glo_storageFactor;
extern int loc_storageFactor;
extern int reservoir_dsc; //downstream cells that are considered for water use of reservoir (for 5min always the same)
extern double defaultRiverVelocity;



extern void initModel(List ListConst);

#endif