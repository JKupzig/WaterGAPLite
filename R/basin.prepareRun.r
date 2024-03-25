#' @title Preparation of model run (in rcpp)
#' @description Function to create List to pass to WGL
#' @param basin_object basinObject defined from init.model() 
#' @param climate_object climateObject defined from init.climate()
#' @param wateruse_object waterUseObject defined from init.waterUse()
#' @return List to pass to rcpp function to run model
#' @export
basin.prepare_run <- function(basin_object,
                             climate_object,
                             wateruse_object) {

  dateformat <- "%d.%m.%Y"
  startstring <- slot(climate_object, "start")
  endstring <- slot(climate_object, "end")

  #first checking dates
  if (startstring != slot(wateruse_object, "start")) {
    stop("timeseries from waterUse and Climate are not 
          refering to the same period!")
  }

  if (endstring != slot(wateruse_object, "end")) {
        stop("timeseries from waterUse and Climate are not 
          refering to the same period!")
  }

  #getting timeseries to simulate
  sim_period_date <- seq(as.Date(startstring, format = dateformat),
                          as.Date(endstring, format = dateformat),
                          1)

  basin_list <- list() #list to pass to wgl

  basin_list[["SystemValuesPath"]] <- basin_object@cont@SystemValues
  basin_list[["id"]] <- basin_object@id

  basin_list[["SimPeriod"]] <- sim_period_date
  basin_list[["temp"]] <- climate_object@temp
  basin_list[["shortwave"]] <- climate_object@shortwave
  basin_list[["longwave"]] <- climate_object@longwave
  basin_list[["prec"]] <- climate_object@prec

  basin_list[["cor_row"]] <- slot(basin_object, "cont")@corRow
  basin_list[["GR"]] <- basin_object@GR
  basin_list[["NeighbouringCells"]] <- basin_object@NeighbouringCells

  basin_list[["albedo"]] <- basin_object@albedo
  basin_list[["albedoSnow"]] <- basin_object@albedoSnow
  basin_list[["emissivity"]] <- basin_object@emissivity
  basin_list[["alphaPT"]] <- basin_object@alphaPT
  basin_list[["maxDailyPET"]] <- basin_object@maxDailyPET

  basin_list[["LAI_min"]] <- basin_object@LAI_min
  basin_list[["LAI_max"]] <- basin_object@LAI_max
  basin_list[["initDays"]] <- basin_object@initDays
  basin_list[["GLCT"]] <- basin_object@GLCT

  basin_list[["G_ELEV_RANGE.26"]] <- basin_object@G_ELEV_RANGE.26
  basin_list[["GBUILTUP"]] <- basin_object@GBUILTUP

  basin_list[["array_size"]] <- basin_object@array_size


  basin_list[["maxCanopyStoragePerLAI"]] <- basin_object@maxCanopyStoragePerLAI
  basin_list[["canopyEvapoExp"]] <- basin_object@canopyEvapoExp

  basin_list[["degreeDayFactor"]] <- basin_object@degreeDayFactor
  basin_list[["snowFreezeTemp"]] <- basin_object@snowFreezeTemp
  basin_list[["snowMeltTemp"]] <- basin_object@snowMeltTemp

  basin_list[["runoffFracBuiltUp"]] <- basin_object@runoffFracBuiltUp
  basin_list[["G_GAMMA_HBV"]] <- basin_object@G_GAMMA_HBV
  
  basin_list[["lower_threshold_soil"]] <- basin_object@lower_threshold_soil
  basin_list[["G_Smax"]] <- basin_object@G_Smax
  basin_list[["G_ARID_HUMID"]] <- basin_object@G_ARID_HUMID
  basin_list[["G_TEXTURE"]] <- basin_object@G_TEXTURE
  basin_list[["G_gwFactor"]] <- basin_object@G_gwFactor
  basin_list[["pcrit"]] <- basin_object@pcrit
  basin_list[["G_LOCLAK"]] <- basin_object@G_LOCLAK
  basin_list[["G_LOCWET"]] <- basin_object@G_LOCWET
  basin_list[["G_GLOLAK"]] <- basin_object@G_GLOLAK
  basin_list[["G_GLOWET"]] <- basin_object@G_GLOWET
  basin_list[["G_RESAREA"]] <- basin_object@G_RESAREA
  basin_list[["G_LAKAREA"]] <- basin_object@G_LAKAREA
  basin_list[["routeOrder"]] <- basin_object@routeOrder
  basin_list[["GAREA"]] <- basin_object@GAREA
  basin_list[["landfrac"]] <- basin_object@landfrac
  basin_list[["lakeDepth"]] <- basin_object@lakeDepth
  basin_list[["lakeOutflowExp"]] <- basin_object@lakeOutflowExp
  basin_list[["G_LAKAREA"]] <- basin_object@G_LAKAREA
  basin_list[["wetlandDepth"]] <- basin_object@wetlandDepth
  basin_list[["wetlOutflowExp"]] <- basin_object@wetlOutflowExp
  basin_list[["evapoReductionExp"]] <- basin_object@evapoReductionExp
  basin_list[["loc_storageFactor"]] <- basin_object@loc_storageFactor
  basin_list[["glo_storageFactor"]] <- basin_object@glo_storageFactor
  basin_list[["k_g"]] <- basin_object@k_g
  basin_list[["outflow"]] <- basin_object@outflow
  basin_list[["G_RG_max"]] <- basin_object@G_RG_max
  basin_list[["G_STORAGE_CAPACITY"]] <- basin_object@G_STORAGE_CAPACITY
  basin_list[["G_MEAN_INFLOW"]] <- basin_object@G_MEAN_INFLOW
  basin_list[["G_START_MONTH"]] <- basin_object@G_START_MONTH
  basin_list[["evapoReductionExpReservoir"]] <- basin_object@evapoReductionExpReservoir
  basin_list[["G_RES_TYPE"]] <- basin_object@G_RES_TYPE
  basin_list[["G_ALLOC_COEFF.20"]] <- basin_object@G_ALLOC_COEFF.20
  basin_list[["Splitfactor"]] <- basin_object@Splitfactor

  basin_list[["G_riverLength"]] <- basin_object@G_riverLength
  basin_list[["G_riverSlope"]] <- basin_object@G_riverSlope
  basin_list[["G_riverRoughness"]] <- basin_object@G_riverRoughness
  basin_list[["G_BANKFULL"]] <- basin_object@G_BANKFULL

  basin_list[["defaultRiverVelocity"]] <- basin_object@defaultRiverVelocity

  basin_list[["Info_GW"]] <- wateruse_object@Info_GW
  basin_list[["Info_SW"]] <- wateruse_object@Info_SW
  basin_list[["Info_TF"]] <- wateruse_object@Info_TF
  basin_list[["G_NUs_7100"]] <- wateruse_object@G_NUs_7100
  
  basin_list[["snow_threshold"]] <- -5.
  basin_list[["max_degree_days"]] <- 10

  return(basin_list)
}
