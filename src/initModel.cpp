#include <Rcpp.h>
#include "initModel.h"

using namespace Rcpp;
using namespace std;

// Declaration of global constant variables that do not change in the whole simulation (static variables)
// this makes a shallow copy, with the R object underlying
// when the object do not change there is not much additional storage needed
// because the object are referred to as "const" they can't change in the whole script! --> I think everything is fine
// ATTENTION: redefinition of object name is possible, so when list is redefined than it is actually possible to change objects }


//SETTINGS
int waterUseType;
int flowVelocityType;
int GapYearType;
int WaterUseAllocationType;
int ReservoirType;
int splitType;
int calcLong;
int useSystemVals;

//CONSTANT FILES

String SystemValues;
int id;

NumericMatrix Temp;
NumericMatrix Rs;
NumericMatrix Rl;
NumericMatrix Prec; //WaterContent at the end of forstep
int cor_row;
NumericMatrix G_Elevation; //elevation of grid and subgrids
NumericMatrix NeighbouringCells;

IntegerVector GR;

NumericMatrix Info_GW;
NumericMatrix Info_SW;
NumericMatrix Info_TF;
NumericVector YearlyMeanDemand;

NumericVector LAI_max;
NumericVector GLCT;
NumericVector initDays;
NumericVector LAI_min;
NumericMatrix dailyLaiAll; //Maximal Interception Storage

NumericVector albedo;
NumericVector albedoSnow;
NumericVector emissivity;
NumericVector alphaPT;
NumericVector degreeDayFactor;
NumericVector GBUILTUP;
NumericVector G_GAMMA_HBV; //calibrated gamma value
NumericVector maxDailyPET; ////precipitation + snow melt that comes to soil
NumericVector G_Smax ; //size of soil layer/storage
IntegerVector G_ARID_HUMID;
NumericVector G_TEXTURE;
NumericVector G_RG_max;
NumericVector G_gwFactor;
NumericVector GAREA;
NumericVector landfrac;
NumericVector G_ALLOC_COEFF;
IntegerVector G_LOCLAK; // % of cell that belongs to local lake
IntegerVector G_LOCWET; // % of cell that belongs to local wetland
IntegerVector G_GLOLAK; // % of cell that belongs to global lake (including resevroirs at the moment)
IntegerVector G_GLOWET; // % of cell that belongs to global wetland
NumericVector G_RESAREA; // km² reservoir area defined in outlet cell of reservoirs
NumericVector G_LAKAREA; // km² global lake area defined in outlet cell of global lake
NumericVector G_STORAGE_CAPACITY;
NumericVector G_MEAN_INFLOW;
IntegerVector G_START_MONTH;
IntegerVector G_RES_TYPE;
IntegerVector routeOrder;
IntegerVector outflowOrder; // obtained from routing input, modified

NumericVector G_riverLength;
NumericVector G_BANKFULL;
NumericVector G_riverSlope;
NumericVector G_riverRoughness;

NumericVector Splitfactor;

double maxCanopyStoragePerLAI; // 0.3 mm
double canopyEvapoExp; // 0.6666667 [-]
int array_size; //
double snowFreezeTemp; // 0°C
double snowMeltTemp;  // 0
double runoffFracBuiltUp; // 0.5
double  pcrit;
double  k_g;
double lakeDepth; // 0.005 km --> 5000 mm
double lakeOutflowExp; // 1.5 [-]
double wetlandDepth; // 0.002 km --> 2000 mm
double wetlOutflowExp; // 2.5 [-]
double evapoReductionExp; // 3.32193
double evapoReductionExpReservoir; // 2.81383
int glo_storageFactor;
int loc_storageFactor;
int reservoir_dsc = 20; //downstream cells that are considered for water use of reservoir (for 5min always the same)
double defaultRiverVelocity; // = 86.4;	// [km/d] = 1 m/s

//' @title Declaration of Settings from R Module
//' @description translates R Settings to global rcpp Settings
//' @param Settings Settings defined as IntegerVector
//' @export
void defSettings(NumericVector Settings){

	// check for correct settings input
	if (Settings.size() != 8)
	{
		stop("Settings should be a vector of length 8");
	}
	if (Settings[0] != 0 && Settings[0] != 1 && Settings[0] != 2)
	{
		stop("WaterUseType should be 0, 1 or 2");
	}
	if (Settings[1] != 0 && Settings[1] != 1 && Settings[1] != 2)
	{
		stop("WaterUseAllocationType should be 0, 1 or 2");
	}
	if (Settings[2] != 0 && Settings[2] != 1)
	{
		stop("flowVelocityType should be 0 or 1");
	}
	if (Settings[3] != 0 && Settings[3] != 1)
	{
		stop("GapYearType should be 0 or 1");
	}
	if (Settings[4] != 0 && Settings[4] != 1)
	{
		stop("ReservoirType should be 0 or 1");
	}
	if (Settings[5] != 0 && Settings[5] != 1)
	{
		stop("splitttingFactor parameter should be 0 or 1");
	}
	if (Settings[6] != 0 && Settings[6] != 1)
	{
		stop("calculation LongWave parameter should be 0 or 1");
	}
	if (Settings[7] != 0 && Settings[7] != 1 && Settings[7] != 2 && Settings[7] != 3)
	{
		stop("useSystemVals should be 0, 1, 2 or 3");
	}

	waterUseType = Settings[0];
	WaterUseAllocationType = Settings[1];
	flowVelocityType = Settings[2];
	GapYearType = Settings[3];
	ReservoirType = Settings[4];
	splitType = Settings[5];
	calcLong = Settings[6];
	useSystemVals = Settings[7];
}

//' @title detLAIdaily
//' @description Definition of interception storage size
//' @param LAI_min minimal interception storage in mm from LAI_info
//' @param LAI_max maximal interception storage in mm from LAI_info
//' @param initDays needed days to start growing season in d from LAI_info
//' @param Temp Temperature in °C (Matrix)
//' @param Prec Precipitation in mm (Matrix)
//' @param aridType arid or humid tyoe definition for cells, obtained from G_ARID_HUMID.UNF
//' @param GLCT Landcover information
//' @return Matrix with interception storage in mm (rows=days, cols=cells)
//' @export

NumericMatrix getLAIdaily(NumericVector LAI_min, NumericVector LAI_max, NumericVector initDays,
						const NumericMatrix Temp, const NumericMatrix Prec, const IntegerVector aridType, const NumericVector GLCT){
  //Ths function uses the model function as it is implemented iin WG3.1

  int nrow = Temp.nrow();
  int ncol = Temp.ncol();

  NumericMatrix dailyLAI (nrow, ncol);
  IntegerMatrix growingSeason (nrow, ncol);

  const double Tgrenz = 8.0; //as in model lai.cpp ll. 169
  const double PrecSumgrenz = 40;

	//this is the original model code implemented, however the output seems to be quite weird..
  for (int col = 0; col < ncol; col++){ //iterating through cells
	//for each cell everything is set to zero at the beginning of evaluation
	double PrecSum = 0;
	int GrowingStatus = 0;
	int days_since_start = 0;

	//iterating through days
	for (int row = 0; row < nrow; row++){
		if (Temp(row, col) > Tgrenz){ //case 1
			if (GrowingStatus == 0) { // vegetation is not grown yet...
				if (days_since_start >= initDays[col]){ // ...but initial days of growing season have been reached
					days_since_start++;
					PrecSum += Prec(row,col);
					if(PrecSum > PrecSumgrenz ){ // threshold of min precipitation has been met and growing season can start
						if (days_since_start >= initDays[col] + 30){ // full vegetation development has been reached and status will switch
							days_since_start = initDays[col] + 30;
							GrowingStatus = 1;
						}
						// return either the LAI in sprout or the max LAI-value
						dailyLAI(row,col) = (LAI_min[col] + (LAI_max[col] - LAI_min[col]) * (days_since_start - initDays[col]) / 30.);
					} else { // sum of precipitation is not high enough to start growing season
						days_since_start = initDays[col];
						dailyLAI(row,col) = (LAI_min[col]);
					}
				} else { // initial days of growing season have not been reached yet
					days_since_start++;
					PrecSum += Prec(row,col);
					dailyLAI(row,col) = (LAI_min[col]);
				}
			} else{// vegetation is already fully grown...
				if (days_since_start <= 30) { // ...and we are in phase of senescence
					days_since_start--;
					if (GLCT[col] <= 2){ // if land_cover_type is '1' or '2' we have ervergreen plants and LAI will never clompletely degrade JK: not sure why this is done because LAImin is quite high for these plants
						GrowingStatus = 0; // therefore status will switch at once
					}
					if (days_since_start <= 0){ // LAI is completely degraded to min LAI and status will switch
						days_since_start = 0;
						GrowingStatus = 0;
						PrecSum = 0.;
					}
					dailyLAI(row,col) = (LAI_max[col] - (LAI_max[col] - LAI_min[col]) * (30 - days_since_start) / 30.);
				} else { // initial days for senescence phase have not been reached yet and we have growing conditions (again)
					if ( (aridType[col] != 1) & (Prec(row,col)<0.5) ){ // in arid regions we have no growing conditions if there is no rain
						days_since_start--;
					} else {// if conditions for LAI reduction should happen day after day, use this  equation (reset for initial days)
						days_since_start = 30 + initDays[col];
					}
					dailyLAI(row,col) = (LAI_max[col]);
				}
			}
		} else { //case 2
			if (GrowingStatus == 0) { // vegetation is not fully grown yet
				if (days_since_start > initDays[col]){ // initial days of growing season have been reached and plants will grow anyway
					(days_since_start)++;
					(PrecSum) += Prec(row,col);
					if(PrecSum > PrecSumgrenz){ // threshold of min precipitation has been met and growing season can start
						if (days_since_start >= initDays[col] + 30){
							days_since_start = initDays[col] + 30;
							GrowingStatus = 1;
						}
						dailyLAI(row,col) = (LAI_min[col] + (LAI_max[col] - LAI_min[col]) * (days_since_start - initDays[col]) / 30.);
					} else { // sum of precipitation is not high enough to start growing season
						days_since_start = initDays[col];
						dailyLAI(row,col) = (LAI_min[col]);
					}
				} else { // no growing season
					(PrecSum) += Prec(row,col);
					dailyLAI(row,col) = (LAI_min[col]);
				}
			} else { // we are in growing season but lai will be reduced now
				if (days_since_start <= 30) {
					(days_since_start)--;
					if (days_since_start <= 0){
						days_since_start = 0;
						GrowingStatus = 0;
						PrecSum = 0.;
					}
					dailyLAI(row,col) = (LAI_max[col] - (LAI_max[col] - LAI_min[col]) * (30 - days_since_start) / 30.);
				} else { // we are in growing season but the inital days for lai degrading have not been met
					(days_since_start)--;
					dailyLAI(row,col) = (LAI_max[col]);
				}
			}
		}
	}
  }
  return(dailyLAI);
}


//' @title initModel
//' @description Sets passed List as global model input
//' @param ListConst that is defined in R
//' @export
void initModel(List ListConst){

	SystemValues = as<String>(ListConst["SystemValuesPath"]);
	id = as<int>(ListConst["id"]);

	Temp = as<NumericMatrix>(ListConst["temp"]);
	Rs = as<NumericMatrix>(ListConst["shortwave"]);
	Rl = as<NumericMatrix>(ListConst["longwave"]);
	Prec = as<NumericMatrix>(ListConst["prec"]);
	GR = as<IntegerVector>(ListConst["GR"]);

	G_Elevation = as<NumericMatrix>(ListConst["G_ELEV_RANGE.26"]);
	NeighbouringCells = as<NumericMatrix>(ListConst["NeighbouringCells"]);

	LAI_max = as<NumericVector>(ListConst["LAI_max"]);
	LAI_min = as<NumericVector>(ListConst["LAI_min"]);
	initDays = as<NumericVector>(ListConst["initDays"]);
	GLCT = as<NumericVector>(ListConst["GLCT"]);

	albedo = as<NumericVector>(ListConst["albedo"]);
	albedoSnow = as<NumericVector>(ListConst["albedoSnow"]);
	emissivity = as<NumericVector>(ListConst["emissivity"]);
	alphaPT = as<NumericVector>(ListConst["alphaPT"]);
	degreeDayFactor = as<NumericVector>(ListConst["degreeDayFactor"]);
	GBUILTUP = as<NumericVector>(ListConst["GBUILTUP"]);
	G_GAMMA_HBV = as<NumericVector>(ListConst["G_GAMMA_HBV"]);
	maxDailyPET = as<NumericVector>(ListConst["maxDailyPET"]);
	G_Smax = as<NumericVector>(ListConst["G_Smax"]);
	G_ARID_HUMID = as<IntegerVector>(ListConst["G_ARID_HUMID"]);
	G_TEXTURE = as<NumericVector>(ListConst["G_TEXTURE"]);
	G_gwFactor = as<NumericVector>(ListConst["G_gwFactor"]);
	G_RG_max = as<NumericVector>(ListConst["G_RG_max"]);
	GAREA = as<NumericVector>(ListConst["GAREA"]);
	landfrac = as<NumericVector>(ListConst["landfrac"]);
	G_ALLOC_COEFF = as<NumericVector>(ListConst["G_ALLOC_COEFF.20"]);
	G_LOCLAK = as<IntegerVector>(ListConst["G_LOCLAK"]);
	G_LOCWET = as<IntegerVector>(ListConst["G_LOCWET"]);
	G_GLOLAK = as<IntegerVector>(ListConst["G_GLOLAK"]);
	G_GLOWET = as<IntegerVector>(ListConst["G_GLOWET"]);
	G_RESAREA = as<NumericVector>(ListConst["G_RESAREA"]);
	G_LAKAREA = as<NumericVector>(ListConst["G_LAKAREA"]);
	G_STORAGE_CAPACITY = as<NumericVector>(ListConst["G_STORAGE_CAPACITY"]);
	G_MEAN_INFLOW = as<NumericVector>(ListConst["G_MEAN_INFLOW"]);
	G_START_MONTH = as<IntegerVector>(ListConst["G_START_MONTH"]);
	G_RES_TYPE = as<IntegerVector>(ListConst["G_RES_TYPE"]);
	routeOrder = as<IntegerVector>(ListConst["routeOrder"]);
	outflowOrder = as<IntegerVector>(ListConst["outflow"]);

	G_riverLength = as<NumericVector>(ListConst["G_riverLength"]);
	G_BANKFULL = as<NumericVector>(ListConst["G_BANKFULL"]);
	G_riverSlope = as<NumericVector>(ListConst["G_riverSlope"]);
	G_riverRoughness = as<NumericVector>(ListConst["G_riverRoughness"]);

	Splitfactor = as<NumericVector>(ListConst["Splitfactor"]);

	Info_GW = as<NumericMatrix>(ListConst["Info_GW"]);
	Info_SW = as<NumericMatrix>(ListConst["Info_SW"]);
	Info_TF = as<NumericMatrix>(ListConst["Info_TF"]);
	YearlyMeanDemand = as<NumericVector>(ListConst["G_NUs_7100"]);

	maxCanopyStoragePerLAI = as<double>(ListConst["maxCanopyStoragePerLAI"]);
	canopyEvapoExp = as<double>(ListConst["canopyEvapoExp"]);
	array_size = as<int>(ListConst["array_size"]);
	snowFreezeTemp = as<double>(ListConst["snowFreezeTemp"]);
	snowMeltTemp = as<double>(ListConst["snowMeltTemp"]);
	runoffFracBuiltUp = as<double>(ListConst["runoffFracBuiltUp"]);
	pcrit = as<double>(ListConst["pcrit"]);
	k_g = as<double>(ListConst["k_g"]);
	lakeDepth = as<double>(ListConst["lakeDepth"]);
	lakeOutflowExp = as<double>(ListConst["lakeOutflowExp"]);
	wetlandDepth = as<double>(ListConst["wetlandDepth"]);
	wetlOutflowExp = as<double>(ListConst["wetlOutflowExp"]);
	evapoReductionExp = as<double>(ListConst["evapoReductionExp"]);
	evapoReductionExpReservoir = as<double>(ListConst["evapoReductionExpReservoir"]);
	glo_storageFactor = as<int>(ListConst["glo_storageFactor"]);
	loc_storageFactor = as<int>(ListConst["loc_storageFactor"]);
	cor_row = as<int>(ListConst["cor_row"]);

	defaultRiverVelocity = as<double>(ListConst["defaultRiverVelocity"]);

	dailyLaiAll = getLAIdaily(LAI_min, LAI_max, initDays,
						   Temp, Prec, G_ARID_HUMID, GLCT);

}



