#' @title Preparation of climate object for model run
#' @description Function has to be excecuted after basin initialization.
#' It loads every required meteorological input
#' @param basin_object that defines basin for which climate is loaded
#' @param start date as string of type \%dd.\%mm.\%YYYY, e.g. 01.01.1980
#' @param end date as string of type \%dd.\%mm.\%YYYY, e.g. 01.01.1980
#' @param force2read to force model to read climate data again, Climateshortcut
#' is first ignored and then updated
#' @param climate_format to define which data type should be used
#' (use 'global' or 'continental' or 'basin')
#' @return new Climate object with matrixes for tempertaure, precipitation,
#' longwave and shortwave downard radiation (each row is one day,
#' and each column is one cell in basin)
#' @export

init.climate <- function(basin_object, start, end,
                         force2read = FALSE, climate_format = "global") {

  cont <- slot(basin_object, "cont")@contName
  climate_shortcut <- slot(basin_object, "cont")@climateShortcut
  data_dir <- slot(basin_object, "cont")@DataDir

  basin_index <- basin_object@basinIndex
  grdc_number <- basin_object@id
  wg2with5min_mask <- basin_object@G_WG3_WG2WITH5MIN
  watch_mask <- basin_object@G_WG3_WATCH
  trans_matrix <-basin_object@gcrcWithoutZero

  simPeriodDate <- seq(as.Date(start, format = "%d.%m.%Y"),
                      as.Date(end, format = "%d.%m.%Y"), 1)

  prec <- basin.loading_climate(cont, grdc_number, basin_index, trans_matrix,
                                simPeriodDate, climate_shortcut, data_dir,
                                wg2with5min_mask, watch_mask,
                               type_name = "prec", read_newly = force2read,
                               climate_format)

  temp <- basin.loading_climate(cont, grdc_number, basin_index, trans_matrix,
                              simPeriodDate, climate_shortcut, data_dir,
                              wg2with5min_mask, watch_mask,
                              type_name = "temp", read_newly = force2read,
                              climate_format)

  longwave   <- basin.loading_climate(cont, grdc_number, basin_index,
                              trans_matrix,
                              simPeriodDate, climate_shortcut, data_dir,
                              wg2with5min_mask, watch_mask,
                              type_name = "longwave", read_newly=force2read,
                              climate_format)

  shortwave   <- basin.loading_climate(cont, grdc_number, basin_index,
                              trans_matrix,
                              simPeriodDate, climate_shortcut, data_dir,
                              wg2with5min_mask, watch_mask,
                              type_name = "shortwave", read_newly = force2read,
                              climate_format)

  newClimate <- new("Climate",
                 start = start,
                 end = end,
                 temp = temp,
                 prec = prec,
                 shortwave = shortwave,
                 longwave = longwave)

  return(newClimate)

}
