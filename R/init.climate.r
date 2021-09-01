#' @title Preparation of climate object for model run
#' @description Function has to be excecuted after basin initialization. It loads every required meteorological input
#' 
#' @param basinObject that defines basin for which climate is loaded
#' @param start date as string of type \%dd.\%mm.\%YYYY, e.g. 01.01.1980
#' @param end date as string of type \%dd.\%mm.\%YYYY, e.g. 01.01.1980
#' @param force2read to force model to read climate data again, Climateshortcut is first ignored and then updated 
#' @param ClimateFormat to define which data type should be used (use 'global' or 'continental' or 'basin')
#' @return new Climate object with matrixes for tempertaure, precipitation longwave and shortwave downard radiation (each row is one day, and each column is one cell in basin)
#' @export

init.climate <- function(basinObject, start, end, force2read=F, ClimateFormat="global"){

  cont <- slot(basinObject, "cont")@contName
  climateShortcut <- slot(basinObject, "cont")@climateShortcut
  DataDir <- slot(basinObject, "cont")@DataDir

  basinIndex <- basinObject@basinIndex
  grdc_number <- basinObject@id
  G_WG3_WG2WITH5MIN <- basinObject@G_WG3_WG2WITH5MIN
  G_WG3_WATCH <- basinObject@G_WG3_WATCH
  transMatrix <-basinObject@gcrcWithoutZero

  simPeriodDate <- seq(as.Date(start, format="%d.%m.%Y"), as.Date(end, format="%d.%m.%Y"),1)

  prec <- basin.loadingClimate(cont, grdc_number, basinIndex, transMatrix, simPeriodDate,
                               climateShortcut, DataDir, G_WG3_WG2WITH5MIN, G_WG3_WATCH,
                               typeName="prec", readNewly=force2read, ClimateFormat)
  temp <- basin.loadingClimate(cont, grdc_number, basinIndex, transMatrix, simPeriodDate,
                               climateShortcut, DataDir, G_WG3_WG2WITH5MIN, G_WG3_WATCH,
                               typeName="temp", readNewly=force2read,ClimateFormat)
  lw   <- basin.loadingClimate(cont, grdc_number, basinIndex, transMatrix, simPeriodDate,
                               climateShortcut, DataDir, G_WG3_WG2WITH5MIN, G_WG3_WATCH,
                               typeName="longwave", readNewly=force2read, ClimateFormat)
  sw   <- basin.loadingClimate(cont, grdc_number, basinIndex, transMatrix, simPeriodDate,
                               climateShortcut, DataDir, G_WG3_WG2WITH5MIN, G_WG3_WATCH,
                               typeName="shortwave", readNewly=force2read,ClimateFormat)

  newClimate <- new("Climate",
                 start=start,
                 end=end,
                 temp=temp,
                 prec=prec,
                 shortwave=sw,
                 longwave=lw)

  return(newClimate)

}
