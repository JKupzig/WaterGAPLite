#' @title Preparation of model run (in rcpp)
#' @description Function to create List to pass to WGL
#' @param basinObject basinObject defined from init.model() 
#' @param climateObject climateObject defined from init.climate()
#' @param waterUseObject waterUseObject defined from init.waterUse()
#' @return List to pass to rcpp function to run model
#' @export

basin.prepareRun <- function(basinObject,
                             climateObject,
                             waterUseObject){

  #first checking dates
  if ((slot(climateObject, "start") != slot(waterUseObject, "start")) | (slot(climateObject, "end") != slot(waterUseObject, "end"))) {
    stop("timeseries from waterUse and Climate are not refering to the same period!")
  } else {
    simPeriodDate <- seq(as.Date(slot(climateObject, "start"), format="%d.%m.%Y"), as.Date(slot(climateObject, "end"), format="%d.%m.%Y"),1) #getting timeseries to simulate
  }

  ListConst = list() #list to pass to wgl
  
  ListConst[["SystemValuesPath"]] = basinObject@cont@SystemValues
  ListConst[["id"]] = basinObject@id

  ListConst[["SimPeriod"]] <- simPeriodDate
  ListConst[["temp"]] = climateObject@temp
  ListConst[["shortwave"]] = climateObject@shortwave
  ListConst[["longwave"]] = climateObject@longwave
  ListConst[["prec"]] = climateObject@prec
  
  ListConst[["cor_row"]] = slot(basinObject, "cont")@corRow
  ListConst[["GR"]] = basinObject@GR
  ListConst[["NeighbouringCells"]] = basinObject@NeighbouringCells

  ListConst[["albedo"]] = basinObject@albedo
  ListConst[["albedoSnow"]] = basinObject@albedoSnow
  ListConst[["emissivity"]] = basinObject@emissivity
  ListConst[["alphaPT"]] = basinObject@alphaPT
  ListConst[["maxDailyPET"]] = basinObject@maxDailyPET

  ListConst[["LAI_min"]] = basinObject@LAI_min
  ListConst[["LAI_max"]] = basinObject@LAI_max
  ListConst[["initDays"]] = basinObject@initDays
  ListConst[["GLCT"]] = basinObject@GLCT

  ListConst[["G_ELEV_RANGE.26"]] = basinObject@G_ELEV_RANGE.26
  ListConst[["GBUILTUP"]] = basinObject@GBUILTUP

  ListConst[["array_size"]] = basinObject@array_size


  ListConst[["maxCanopyStoragePerLAI"]] = basinObject@maxCanopyStoragePerLAI
  ListConst[["canopyEvapoExp"]] = basinObject@canopyEvapoExp

  ListConst[["degreeDayFactor"]] = basinObject@degreeDayFactor
  ListConst[["snowFreezeTemp"]] = basinObject@snowFreezeTemp
  ListConst[["snowMeltTemp"]] = basinObject@snowMeltTemp

  ListConst[["runoffFracBuiltUp"]] = basinObject@runoffFracBuiltUp
  ListConst[["G_GAMMA_HBV"]] = basinObject@G_GAMMA_HBV #rep(1, array_size) #G_GAMMA_HBV

  ListConst[["G_Smax"]] = basinObject@G_Smax
  ListConst[["G_ARID_HUMID"]] = basinObject@G_ARID_HUMID
  ListConst[["G_TEXTURE"]] = basinObject@G_TEXTURE
  ListConst[["G_gwFactor"]] = basinObject@G_gwFactor
  ListConst[["pcrit"]] = basinObject@pcrit
  ListConst[["G_LOCLAK"]] = basinObject@G_LOCLAK
  ListConst[["G_LOCWET"]] = basinObject@G_LOCWET
  ListConst[["G_GLOLAK"]] = basinObject@G_GLOLAK
  ListConst[["G_GLOWET"]] = basinObject@G_GLOWET
  ListConst[["G_RESAREA"]] = basinObject@G_RESAREA
  ListConst[["G_LAKAREA"]] = basinObject@G_LAKAREA
  ListConst[["routeOrder"]] = basinObject@routeOrder
  ListConst[["GAREA"]] = basinObject@GAREA
  ListConst[["landfrac"]] = basinObject@landfrac
  ListConst[["lakeDepth"]] = basinObject@lakeDepth
  ListConst[["lakeOutflowExp"]] = basinObject@lakeOutflowExp
  ListConst[["G_LAKAREA"]] = basinObject@G_LAKAREA
  ListConst[["wetlandDepth"]] = basinObject@wetlandDepth
  ListConst[["wetlOutflowExp"]] = basinObject@wetlOutflowExp
  ListConst[["evapoReductionExp"]] = basinObject@evapoReductionExp
  ListConst[["loc_storageFactor"]] = basinObject@loc_storageFactor
  ListConst[["glo_storageFactor"]] = basinObject@glo_storageFactor
  ListConst[["k_g"]] = basinObject@k_g
  ListConst[["outflow"]] = basinObject@outflow
  ListConst[["G_RG_max"]] = basinObject@G_RG_max
  ListConst[["G_STORAGE_CAPACITY"]] = basinObject@G_STORAGE_CAPACITY
  ListConst[["G_MEAN_INFLOW"]] = basinObject@G_MEAN_INFLOW
  ListConst[["G_START_MONTH"]] = basinObject@G_START_MONTH
  ListConst[["evapoReductionExpReservoir"]] = basinObject@evapoReductionExpReservoir
  ListConst[["G_RES_TYPE"]] = basinObject@G_RES_TYPE  #
  ListConst[["G_ALLOC_COEFF.20"]] = basinObject@G_ALLOC_COEFF.20
  ListConst[["Splitfactor"]] = basinObject@Splitfactor

  ListConst[["G_riverLength"]] = basinObject@G_riverLength
  ListConst[["G_riverSlope"]] = basinObject@G_riverSlope
  ListConst[["G_riverRoughness"]] = basinObject@G_riverRoughness
  ListConst[["G_BANKFULL"]] = basinObject@G_BANKFULL

  ListConst[["defaultRiverVelocity"]] = basinObject@defaultRiverVelocity

  ListConst[["Info_GW"]] = waterUseObject@Info_GW
  ListConst[["Info_SW"]] = waterUseObject@Info_SW
  ListConst[["Info_TF"]] = waterUseObject@Info_TF
  ListConst[["G_NUs_7100"]] = waterUseObject@G_NUs_7100

  return(ListConst)
}
