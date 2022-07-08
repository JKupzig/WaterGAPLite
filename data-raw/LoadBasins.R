library(WaterGAPLite)

LoadBasins <- function(grdc_no=NULL, ...){

  #settings
  Settings = c(0, # WaterUse --> 0=off, 1=on 2=on (including water transport to cities)
               1, # WaterUseAllocation --> 0: temporal&spatial distribution 1:spatial distribution 2: temporal distribution
               0, # flowVelocity --> 0=const, 1=variable
               0, # GapYear --> 0=With 29.02, 1=Without 29.02 (not used in WGL at the moment)
               1, # reservoirType --> 0: hanasaki, 1: global lakes
               0, # splitting factor --> 0: calculating splitting factor as defined in WG3, 1: setting splitting factor with list (for calibration purpose)
               0, # 0: longwave radiation is read in; 1: Longwave is estimated by incoming shortwave radiaton
               0) # 0: no system values are used, 1: system values are read in, 2: system values are written out, 3: system values are read in and written out

  #General-Information
  start = "01.01.1980" #global variable - at the moment only month is considered not day
  end   = "31.12.1989" #global variable
  base <- r"(C:\Users\jenny\MyProject_sciebo\WaterGAPlite)" #"U:/Wissenschaft/MyProject_sciebo/MyProject/WaterGAPlite"


  kwargs = list(...)
  if ((length(kwargs) == 0) | (length(names(kwargs)) == 0)) {
    print("using default settings:")
    print("01.01.1980 - 31.12.1989")
    print(sprintf('base = %s', base))
    print('Settings = c(0,1,0,0,1,0,0,0)')
  } else {

    start <- ifelse( "start" %in% names(kwargs), kwargs[["start"]], start)
    end <- ifelse( "end" %in% names(kwargs), kwargs[["end"]], end)
    base <- ifelse( "base" %in% names(kwargs), kwargs[["base"]], base)
    Settings <- if ("Settings" %in% names(kwargs)) kwargs[["Settings"]] else Settings

    print("using modified settings:")
    print(sprintf("%s - %s", start, end))
    print(sprintf('base = %s', base))
    print(sprintf('Settings = c(%s)', toString(Settings)))
  }

  checkList <- function(v, entry) {
    return(!entry %in% v)
  }

  if (!is.null(grdc_no)) {
    basinData <- read.csv(r"(C:\Users\jenny\MyProject_sciebo\_SensitvityAnalysis\CaseStudy\Basins\overviewBasins.txt)")

    if (checkList(basinData$grdc_no, grdc_no)){
      stop(sprintf("used basin number %g is not a case study", grdc_no))
    }

    i <- which(basinData$grdc_no == grdc_no)
    bas <- init.model(grdc_number=basinData[i,2], lat=basinData[i,5], long=basinData[i,4], cont=as.character(basinData[i,7]), base)
    bas.climate <- init.climate(bas, start, end, force2read=F, ClimateFormat = "global")
    bas.waterUse <- init.waterUse(bas, start, end, WaterUse_Setting=Settings[1])
    bas.model <- basin.prepareRun(bas, bas.climate, bas.waterUse) #not defined
    basinList <- bas.model

  } else {
    print("loading all 6 case study basins")
    #Get basin Information
    basinData <- read.csv(r"(C:\Users\jenny\MyProject_sciebo\_SensitvityAnalysis\CaseStudy\Basins\overviewBasins.txt)")

    basinList <- list()
    for (i in 2:nrow(basinData)) {

      print(basinData[i,2])
      bas <- init.model(grdc_number=basinData[i,2], lat=basinData[i,5], long=basinData[i,4], cont=as.character(basinData[i,7]), base)
      bas.climate <- init.climate(bas, start, end, force2read=F, ClimateFormat = "global")
      bas.waterUse <- init.waterUse(bas, start, end, WaterUse_Setting=Settings[1])
      bas.model <- basin.prepareRun(bas, bas.climate, bas.waterUse) #not defined

      basinList[[as.character(basinData[i,2])]] <- bas.model
    }
  }

  return(basinList)
}

MockData = LoadBasins()

Basin_1159511 = MockData[["1159511"]]
Basin_1547300 = MockData[["1547300"]]
Basin_4147050 = MockData[["4147050"]]
Basin_4148955 = MockData[["4148955"]]
Basin_2588200 = MockData[["2588200"]]
Basin_4203410 = MockData[["4203410"]]
Basin_6340600 = MockData[["6340600"]]

usethis::use_data(Basin_1159511, overwrite = TRUE,compress ="xz")
usethis::use_data(Basin_1547300, overwrite = TRUE,compress ="xz")
usethis::use_data(Basin_4147050, overwrite = TRUE,compress ="xz")
usethis::use_data(Basin_4148955, overwrite = TRUE,compress ="xz")
usethis::use_data(Basin_2588200, overwrite = TRUE,compress ="xz")
usethis::use_data(Basin_4203410, overwrite = TRUE,compress ="xz")
usethis::use_data(Basin_6340600, overwrite = TRUE,compress ="xz")


