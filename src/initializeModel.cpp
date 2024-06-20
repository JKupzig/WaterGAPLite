#include <Rcpp.h>
#include "initModel.h"
#include "initializeModel.h"

using namespace Rcpp;
using namespace std;


NumericVector G_PETnetShort;
NumericVector G_PETnetLong;

NumericVector daily_prec_to_soil; 
NumericVector dailySoilPET; //left energy for evaporation from soil (PET)
NumericVector dailyCanopyEvapo;
NumericVector dailySnowMelt; //Snowmelt (flux) per day
NumericVector dailySnowEvapo; //Sublimation from snow (flux) per day (no changes between sublimation and evaporation)
NumericVector thresh_elev; //help vector to avoid unlimited snow accumulation in high regions
NumericVector dailyEffPrec; //Water amount that goes to soil (snowmelt + precipitation (T > 0°C)
NumericVector immediate_runoff; //Water amount that is transformed directly to surface run-off
NumericVector dailyAET; // actual evaporation form soil
NumericVector daily_runoff; //amount of sealed ares in grid [-]
NumericVector soil_water_overflow; //amount of sealed ares in grid [-]
NumericVector daily_gw_recharge; 
NumericVector G_dailyLocalSurfaceRunoff;
NumericVector G_dailyLocalGWRunoff;
NumericVector G_dailyUseGW;

//initiliazing storages 
NumericVector G_canopyWaterContent; //canopy storage is defined (0 content)
NumericVector G_snow; //Snow storage for every cell and per day
NumericMatrix G_snowWaterEquivalent; //Snow storage for every subgrid cell and per day
NumericVector G_soilWaterContent; //soil storage
NumericVector G_groundwater; // groundwater storage

//Creating help vectors to define routing order
IntegerVector cellIDs; //cellIDs for actual routing step
IntegerVector numbers; //routing steps 
IntegerVector numbersSorted; 
	
// ROUTING

//Creating working vectors
NumericVector G_riverOutflow; // only for routing, needs ot be set to zero for every day
NumericVector QA_river; //has always river outflow from previous time step 
NumericVector S_river; //has always river inflow from previous time step 
	
NumericVector locLake_overflow;
NumericVector locLake_outflow;
NumericVector S_locLakeStorage;
NumericVector locLake_evapo;
NumericVector locLake_inflow;

NumericVector locWetland_overflow;
NumericVector locWetland_outflow;
NumericVector S_locWetlandStorage;
NumericVector locWetland_evapo;
NumericVector locWetland_inflow;

NumericVector gloLake_overflow;
NumericVector gloLake_outflow;
NumericVector S_gloLakeStorage;
NumericVector gloLake_evapo;
NumericVector gloLake_inflow;

NumericVector Res_outflow;
NumericVector Res_target;
NumericVector S_ResStorage;
NumericVector Res_evapo;
NumericVector Res_inflow;
NumericVector Res_overflow;
NumericVector Res_storage_target;

NumericVector gloWetland_overflow;
NumericVector gloWetland_outflow;
NumericVector S_gloWetlandStorage;
NumericVector gloWetland_evapo;
NumericVector gloWetland_inflow;

NumericMatrix dailyUse; 
NumericVector G_totalUnsatisfiedUse;
NumericVector G_actualUse;
	
	
//' @title Initializing of model
//' @description Vectors and Matrices are initiliazed with the appropiate size for basin (all entries are 0)
void initializeModel(){
	
	G_PETnetShort=NumericVector(array_size);
	G_PETnetLong=NumericVector(array_size);

	daily_prec_to_soil=NumericVector (array_size); 
	dailySoilPET=NumericVector (array_size); //left energy for evaporation from soil (PET)
	dailyCanopyEvapo=NumericVector (array_size);
	dailySnowMelt=NumericVector (array_size); //Snowmelt (flux) per day
	dailySnowEvapo=NumericVector (array_size); //Sublimation from snow (flux) per day (no changes between sublimation and evaporation)
	thresh_elev=NumericVector (array_size); //help vector to avoid unlimited snow accumulation in high regions
	dailyEffPrec=NumericVector (array_size); //Water amount that goes to soil (snowmelt + precipitation (T > 0°C)
	immediate_runoff=NumericVector (array_size); //Water amount that is transformed directly to surface run-off
	dailyAET=NumericVector (array_size); // actual evaporation form soil
	daily_runoff=NumericVector (array_size); //amount of sealed ares in grid [-]
	soil_water_overflow=NumericVector (array_size); //amount of sealed ares in grid [-]
	daily_gw_recharge=NumericVector (array_size); 
	G_dailyLocalSurfaceRunoff=NumericVector (array_size);
	G_dailyLocalGWRunoff=NumericVector (array_size);
	G_dailyUseGW=NumericVector (array_size);

	//initiliazing storages 
	G_canopyWaterContent=NumericVector (array_size); //canopy storage is defined (0 content)
	G_snow=NumericVector (array_size); //Snow storage for every cell and per day
	G_snowWaterEquivalent=NumericMatrix (25, array_size); //Snow storage for every subgrid cell and per day
	G_soilWaterContent=NumericVector (array_size); //soil storage
	G_groundwater=NumericVector (array_size); // groundwater storage
	
	
	
	//Creating help vectors to define routing order
	IntegerVector cellIDs; //cellIDs for actual routing step
	IntegerVector numbers; //routing steps 
	IntegerVector numbersSorted; 
		
	// ROUTING

	//Creating working vectors
	G_riverOutflow=NumericVector (array_size); // only for routing, needs ot be set to zero for every day
	QA_river=NumericVector (array_size); //has always river outflow from previous time step 
	S_river=NumericVector (array_size); //has always river inflow from previous time step 
		
	locLake_overflow=NumericVector (array_size);
	locLake_outflow=NumericVector (array_size);
	S_locLakeStorage=NumericVector (array_size);
	locLake_evapo=NumericVector (array_size);
	locLake_inflow=NumericVector (array_size);

	locWetland_overflow=NumericVector (array_size);
	locWetland_outflow=NumericVector (array_size);
	S_locWetlandStorage=NumericVector (array_size);
	locWetland_evapo=NumericVector (array_size);
	locWetland_inflow=NumericVector (array_size);

	gloLake_overflow=NumericVector (array_size);
	gloLake_outflow=NumericVector (array_size);
	S_gloLakeStorage=NumericVector (array_size);
	gloLake_evapo=NumericVector (array_size);
	gloLake_inflow=NumericVector (array_size);

	Res_outflow=NumericVector (array_size);
	Res_target=NumericVector (array_size);
	Res_target.fill(0);
	S_ResStorage=NumericVector (array_size);
	Res_evapo=NumericVector (array_size);
	Res_inflow=NumericVector (array_size);
	Res_overflow=NumericVector (array_size);
	Res_storage_target=NumericVector (array_size);

	gloWetland_overflow=NumericVector (array_size);
	gloWetland_outflow=NumericVector (array_size);
	S_gloWetlandStorage=NumericVector (array_size);
	gloWetland_evapo=NumericVector (array_size);
	gloWetland_inflow=NumericVector (array_size);
	
	dailyUse=NumericMatrix (2, array_size);
	G_totalUnsatisfiedUse=NumericVector (array_size);
	G_actualUse=NumericVector (array_size);
	
}




