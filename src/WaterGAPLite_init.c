#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _WaterGAPLite_CheckResType();
extern SEXP _WaterGAPLite_createWaterBalance(SEXP);
extern SEXP _WaterGAPLite_dailyEstimateLongwave(SEXP, SEXP, SEXP, SEXP);
extern SEXP _WaterGAPLite_dailyEstimateShortwave(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _WaterGAPLite_dailySnow(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _WaterGAPLite_findNumberInVector(SEXP, SEXP);
extern SEXP _WaterGAPLite_findUniqueValues(SEXP);
extern SEXP _WaterGAPLite_getRiverVelocity(SEXP, SEXP, SEXP);
extern SEXP _WaterGAPLite_numberOfDaysInMonth(SEXP, SEXP);
extern SEXP _WaterGAPLite_numberOfDaysInYear(SEXP);
extern SEXP _WaterGAPLite_routing(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _WaterGAPLite_routingRiver(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _WaterGAPLite_runModel(SEXP, SEXP, SEXP, SEXP);
extern SEXP _WaterGAPLite_setLakeWetlandToMaximum(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _WaterGAPLite_sortIt(SEXP);
extern SEXP _WaterGAPLite_sumVector(SEXP);
extern SEXP _WaterGAPLite_tools_DefDrainageCells(SEXP, SEXP, SEXP);
extern SEXP _WaterGAPLite_tools_InterpolateValues(SEXP, SEXP, SEXP);
extern SEXP _WaterGAPLite_WaterUseCalcDaily(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _WaterGAPLite_WaterUseCalcMeanDemandDaily(SEXP, SEXP);
extern SEXP _WaterGAPLite_WaterUseConsumGW(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_WaterGAPLite_CheckResType",                (DL_FUNC) &_WaterGAPLite_CheckResType,                0},
    {"_WaterGAPLite_createWaterBalance",          (DL_FUNC) &_WaterGAPLite_createWaterBalance,          1},
    {"_WaterGAPLite_dailyEstimateLongwave",       (DL_FUNC) &_WaterGAPLite_dailyEstimateLongwave,       4},
    {"_WaterGAPLite_dailyEstimateShortwave",      (DL_FUNC) &_WaterGAPLite_dailyEstimateShortwave,      5},
    {"_WaterGAPLite_dailySnow",                   (DL_FUNC) &_WaterGAPLite_dailySnow,                   9},
    {"_WaterGAPLite_findNumberInVector",          (DL_FUNC) &_WaterGAPLite_findNumberInVector,          2},
    {"_WaterGAPLite_findUniqueValues",            (DL_FUNC) &_WaterGAPLite_findUniqueValues,            1},
    {"_WaterGAPLite_getRiverVelocity",            (DL_FUNC) &_WaterGAPLite_getRiverVelocity,            3},
    {"_WaterGAPLite_numberOfDaysInMonth",         (DL_FUNC) &_WaterGAPLite_numberOfDaysInMonth,         2},
    {"_WaterGAPLite_numberOfDaysInYear",          (DL_FUNC) &_WaterGAPLite_numberOfDaysInYear,          1},
    {"_WaterGAPLite_routing",                     (DL_FUNC) &_WaterGAPLite_routing,                     5},
    {"_WaterGAPLite_routingRiver",                (DL_FUNC) &_WaterGAPLite_routingRiver,                5},
    {"_WaterGAPLite_runModel",                    (DL_FUNC) &_WaterGAPLite_runModel,                    4},
    {"_WaterGAPLite_setLakeWetlandToMaximum",     (DL_FUNC) &_WaterGAPLite_setLakeWetlandToMaximum,     5},
    {"_WaterGAPLite_sortIt",                      (DL_FUNC) &_WaterGAPLite_sortIt,                      1},
    {"_WaterGAPLite_sumVector",                   (DL_FUNC) &_WaterGAPLite_sumVector,                   1},
    {"_WaterGAPLite_tools_DefDrainageCells",      (DL_FUNC) &_WaterGAPLite_tools_DefDrainageCells,      3},
    {"_WaterGAPLite_tools_InterpolateValues",     (DL_FUNC) &_WaterGAPLite_tools_InterpolateValues,     3},
    {"_WaterGAPLite_WaterUseCalcDaily",           (DL_FUNC) &_WaterGAPLite_WaterUseCalcDaily,           8},
    {"_WaterGAPLite_WaterUseCalcMeanDemandDaily", (DL_FUNC) &_WaterGAPLite_WaterUseCalcMeanDemandDaily, 2},
    {"_WaterGAPLite_WaterUseConsumGW",            (DL_FUNC) &_WaterGAPLite_WaterUseConsumGW,            3},
    {NULL, NULL, 0}
};

void R_init_WaterGAPLite(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}