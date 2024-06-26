#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
   Check these declarations against the C/Fortran source code.
   created with:
   tools::package_native_routine_registration_skeleton(".", character_only = FALSE)
   copy paced to this file
   NEEDED WHEN FUNCTIONS ARE UPDATED
*/

/* .Call calls */
extern SEXP _WaterGAPLite_CheckResType(void);
extern SEXP _WaterGAPLite_createWaterBalance(void *);
extern SEXP _WaterGAPLite_dailyEstimateLongwave(void *, void *, void *, void *);
extern SEXP _WaterGAPLite_dailyEstimateShortwave(void *, void *, void *, void *, void *);
extern SEXP _WaterGAPLite_dailySnow(void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern SEXP _WaterGAPLite_defSettings(void *);
extern SEXP _WaterGAPLite_findNumberInVector(void *, void *);
extern SEXP _WaterGAPLite_findUniqueValues(void *);
extern SEXP _WaterGAPLite_getRiverVelocity(void *, void *, void *);
extern SEXP _WaterGAPLite_numberOfDaysInMonth(void *, void *);
extern SEXP _WaterGAPLite_numberOfDaysInYear(void *);
extern SEXP _WaterGAPLite_routing(void *, void *, void *, void *, void *);
extern SEXP _WaterGAPLite_routingRiver(void *, void *, void *, void *, void *);
extern SEXP _WaterGAPLite_runModel(void *, void *, void *, void *);
extern SEXP _WaterGAPLite_setLakeWetlandToMaximum(void *, void *, void *, void *, void *);
extern SEXP _WaterGAPLite_sortIt(void *);
extern SEXP _WaterGAPLite_sumVector(void *);
extern SEXP _WaterGAPLite_tools_DefDrainageCells(void *, void *, void *);
extern SEXP _WaterGAPLite_tools_interpolate(void *, void *, void *);
extern SEXP _WaterGAPLite_WaterUseCalcDaily(void *, void *, void *, void *, void *, void *, void *, void *);
extern SEXP _WaterGAPLite_WaterUseCalcMeanDemandDaily(void *, void *);
extern SEXP _WaterGAPLite_WaterUseConsumGW(void *, void *, void *);

static const R_CallMethodDef CallEntries[] = {
  {"_WaterGAPLite_CheckResType",                (DL_FUNC) &_WaterGAPLite_CheckResType,                0},
  {"_WaterGAPLite_createWaterBalance",          (DL_FUNC) &_WaterGAPLite_createWaterBalance,          1},
  {"_WaterGAPLite_dailyEstimateLongwave",       (DL_FUNC) &_WaterGAPLite_dailyEstimateLongwave,       4},
  {"_WaterGAPLite_dailyEstimateShortwave",      (DL_FUNC) &_WaterGAPLite_dailyEstimateShortwave,      5},
  {"_WaterGAPLite_dailySnow",                   (DL_FUNC) &_WaterGAPLite_dailySnow,                   9},
  {"_WaterGAPLite_defSettings",                 (DL_FUNC) &_WaterGAPLite_defSettings,                 1},
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
  {"_WaterGAPLite_tools_interpolate",           (DL_FUNC) &_WaterGAPLite_tools_interpolate,           3},
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