#' @title Class Initializing
#' @description Function to define basin and continent
#' class for usage inside the package
#' @importFrom methods setClass

init.classes <- function() {

  class_cache <- new.env(parent = emptyenv())

  setClass("Continent",
           slots = list(
             contName = "character",
             ng = "numeric",
             ng_land = "numeric",
             nrow = "numeric",
             ncol = "numeric",
             xllCorner = "numeric",
             yllCorner = "numeric",
             cellsize = "numeric",
             ng_gcrc = "numeric",
             corRow = "numeric",
             corCol = "numeric",

             DataDir = "character",
             BaseDir = "character",
             OutputDir = "character",
             climateShortcut = "character",
             SystemValues = "character"),

             where = class_cache
           )

  setClass("Climate",
           slots = list(
             start = "character",
             end = "character",
             temp = "matrix",
             shortwave = "matrix",
             longwave = "matrix",
             prec = "matrix"),
             where = class_cache
           )

  setClass("WaterUse",
    slots = list(
      start = "character",
      end = "character",
      Info_GW = "matrix",
      Info_SW = "matrix",
      Info_TF = "matrix",
      G_NUs_7100 = "numeric"),
      where = class_cache
      )

  setClass("Basin",
           slots = list(
             cont = "Continent",
             id = "numeric",
             location = "numeric",

             gcrcWithoutZero = "numeric",
             array_size = "numeric",
             GR = "numeric",
             GC = "numeric",
             GCRC = "numeric",
             NeighbouringCells = "matrix",

             initDays = "numeric",
             LAI_max = "numeric",
             LAI_min = "numeric",
             canopyEvapoExp = "numeric",
             maxCanopyStoragePerLAI = "numeric",

             albedoSnow = "numeric",
             emissivity = "numeric",
             albedo = "numeric",
             alphaPT = "numeric",
             maxDailyPET = "numeric",
             rootingDepth = "numeric",
             degreeDayFactor = "numeric",
             snowFreezeTemp = "numeric",
             snowMeltTemp = "numeric",

             pcrit = "numeric",
             lakeDepth = "numeric",
             runoffFracBuiltUp = "numeric",
             lakeOutflowExp = "numeric",
             wetlOutflowExp = "numeric",
             wetlandDepth = "numeric",
             evapoReductionExp = "numeric",
             loc_storageFactor = "numeric",
             glo_storageFactor = "numeric",
             k_g = "numeric",
             Splitfactor = "numeric",
             defaultRiverVelocity = "numeric",
             evapoReductionExpReservoir = "numeric",

             routeOrder = "numeric",
             outflow = "numeric",
             landfrac = "numeric",
             basinIndex = "numeric",
             G_ELEV_RANGE.26 = "matrix",
             GBUILTUP = "numeric",
             G_Smax = "numeric",
             G_RG_max = "numeric",
             G_gwFactor = "numeric",
             G_GAMMA_HBV = "numeric",
             G_ARID_HUMID = "numeric",
             G_TEXTURE = "numeric",
             G_PERMAGLAC = "numeric",
             G_LOCLAK = "numeric",
             G_LOCWET = "numeric",
             G_GLOLAK = "numeric",
             G_GLOWET = "numeric",
             G_RESAREA = "numeric",
             G_LAKAREA = "numeric",
             GAREA = "numeric",
             G_STORAGE_CAPACITY = "numeric",
             G_MEAN_INFLOW = "numeric",
             G_START_MONTH = "numeric",
             G_RES_TYPE = "numeric",
             G_ALLOC_COEFF.20 = "matrix",
             G_MEAN_INFLOW.12 = "matrix",
             G_ALTITUDE = "numeric",
             G_BATJES = "numeric",
             G_AQ_FACTOR = "numeric",
             GLCT = "numeric",
             G_SLOPE_CLASS = "numeric",

             G_riverLength = "numeric",
             G_riverSlope = "numeric",
             G_riverRoughness = "numeric",
             G_BANKFULL = "numeric",
      G_7daymin = "numeric",
      G_7daymax = "numeric",
      flow_acc = "numeric",
             G_WG3_WG2WITH5MIN = "numeric",
             G_WG3_WATCH = "numeric"
           ), where = class_cache
           )

}
