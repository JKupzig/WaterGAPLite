#' @title Initializing water use information for model run
#' @description function to initialize water uses
#' @param basin_object that defines basin for which water use is loaded
#' @param sim_start as string of Type \%dd.\%mm.\%YYYY, e.g. 01.01.1980
#' @param sim_end as string of Type \%dd.\%mm.\%YYYY, e.g. 01.01.1990
#' @param wateruse_setting 0=noWaterUse is considered
#' and dummy with 0 is loaded, != 1 water use is loaded
#' @return new waterUse object for basin
#' @export


init.wateruse <- function(basin_object,
                          sim_start, sim_end,
                          wateruse_setting = 0) {

  data_dir <- slot(basin_object, "cont")@DataDir
  cont_name <- slot(basin_object, "cont")@contName

  sim_period_date <- seq(as.Date(sim_start, format = "%d.%m.%Y"),
                         as.Date(sim_end, format = "%d.%m.%Y"), 1)

  years <- as.numeric(unique(format(sim_period_date, "%Y")))

  trans_matrix <- basin_object@gcrcWithoutZero
  basin_index <- basin_object@basinIndex
  array_size <- basin_object@array_size

  info_groundwater <- matrix(0, nrow = length(years) * 12, ncol = array_size)
  info_surfacewater <- matrix(0, nrow = length(years) * 12, ncol = array_size)
  info_watertransfer <- matrix(0, nrow = length(years), ncol = array_size)
  wateruse_reference <- rep(0, array_size)
  info_time <- matrix(0, nrow = length(years) * 12, ncol = 2)

  if (wateruse_setting != 0) {

    wateruse_reference <- reading_unf("G_NUs_7100.UNF0",
                                      trans_matrix, basin_index,
                                      name = "waterUse", cont = cont_name)

    i <- 1
    for (year in years) {

      start_index <- (i - 1) * 12 + 1
      end_index <- i * 12
      i <- i + 1

      groundwater <- sprintf("G_NETUSE_GW_HISTAREA_m3_%d.12.UNF0", year)
      info_groundwater[start_index:end_index, ] <- reading_unf(
                                      groundwater,
                                      data_dir, trans_matrix,
                                      basin_index, name = "waterUse",
                                      cont = cont_name)

      surfacewater <- sprintf("G_NETUSE_SW_HISTAREA_m3_%d.12.UNF0", year)
      info_surfacewater[start_index:end_index, ] <- reading_unf(
                          surfacewater, data_dir, trans_matrix,
                          basin_index, name = "waterUse", cont = cont_name)

      water_transfer <- sprintf("G_TRANSFER_DOM_m3_%d.UNF0", year)
      info_watertransfer[i, ] <- reading_unf(
                    water_transfer, data_dir, trans_matrix,
                    basin_index, name = "waterUse", cont = cont_name)

      info_time[start_index:end_index, 1] <- rep(year, 12)
      info_time[start_index:end_index, 2] <- seq(1, 12, 1)
    }
  }

  new_wateruse <- new("WaterUse",
                    start = sim_start,
                    end = sim_end,
                    Info_GW = info_groundwater,
                    Info_SW = info_surfacewater,
                    Info_TF = info_watertransfer,
                    G_NUs_7100 = wateruse_reference)

  return(new_wateruse)

}
