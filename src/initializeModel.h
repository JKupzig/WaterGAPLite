

#ifndef INITIALIZEMODEL_H
#define INITIALIZEMODEL_H
 
#include <Rcpp.h>

using namespace std;
using namespace Rcpp;

void initializeModel();
    
//DAILY 


// This (extern) tells the compile that the variable wat exists somewhere, and that it needs to find it on it's own (in this case, it's in FileB.cpp)
//Creating working vectors
extern NumericVector G_PETnetShort;
extern NumericVector G_PETnetLong;

extern NumericVector daily_prec_to_soil; 
extern NumericVector dailySoilPET; //left energy for evaporation from soil (PET)
extern NumericVector dailyCanopyEvapo;
extern NumericVector dailySnowMelt; //Snowmelt (flux) per day
extern NumericVector dailySnowEvapo; //Sublimation from snow (flux) per day (no changes between sublimation and evaporation)
extern NumericVector thresh_elev; //help vector to avoid unlimited snow accumulation in high regions
extern NumericVector dailyEffPrec; //Water amount that goes to soil (snowmelt + precipitation (T > 0°C)
extern NumericVector immediate_runoff; //Water amount that is transformed directly to surface run-off
extern NumericVector dailyAET; // actual evaporation form soil
extern NumericVector daily_runoff; //amount of sealed ares in grid [-]
extern NumericVector soil_water_overflow; //amount of sealed ares in grid [-]
extern NumericVector daily_gw_recharge; 
extern NumericVector G_dailyLocalSurfaceRunoff;
extern NumericVector G_dailyLocalGWRunoff;
extern NumericVector G_dailyUseGW;

//initiliazing storages 
extern NumericVector G_canopyWaterContent; //canopy storage is defined (0 content)
extern NumericVector G_snow; //Snow storage for every cell and per day
extern NumericMatrix G_snowWaterEquivalent; //Snow storage for every subgrid cell and per day
extern NumericVector G_soilWaterContent; //soil storage
extern NumericVector G_groundwater; // groundwater storage

//Creating help vectors to define routing order
extern IntegerVector cellIDs; //cellIDs for actual routing step
extern IntegerVector numbers; //routing steps 
extern IntegerVector numbersSorted; 
	
// ROUTING

//Creating working vectors
extern NumericVector G_riverOutflow; // only for routing, needs ot be set to zero for every day
extern NumericVector QA_river; //has always river outflow from previous time step 
extern NumericVector S_river; //has always river inflow from previous time step 

extern NumericVector locLake_overflow;
extern NumericVector locLake_outflow;
extern NumericVector S_locLakeStorage;
extern NumericVector locLake_evapo;
extern NumericVector locLake_inflow;

extern NumericVector locWetland_overflow;
extern NumericVector locWetland_outflow;
extern NumericVector S_locWetlandStorage;
extern NumericVector locWetland_evapo;
extern NumericVector locWetland_inflow;

extern NumericVector gloLake_overflow;
extern NumericVector gloLake_outflow;
extern NumericVector S_gloLakeStorage;
extern NumericVector gloLake_evapo;
extern NumericVector gloLake_inflow;

extern NumericVector Res_outflow;
extern NumericVector S_ResStorage;
extern NumericVector Res_evapo;
extern NumericVector Res_inflow;
extern NumericVector Res_overflow;

extern NumericVector gloWetland_overflow;
extern NumericVector gloWetland_outflow;
extern NumericVector S_gloWetlandStorage;
extern NumericVector gloWetland_evapo;
extern NumericVector gloWetland_inflow;

extern NumericMatrix dailyUse;
extern NumericVector G_totalUnsatisfiedUse;
extern NumericVector G_actualUse;

#endif