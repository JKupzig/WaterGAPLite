#' @title  create Raster from vector of basin
#' @description Functions to deal with vectoried basin
#' values in a nice and easy way
#' @param input input vector of basin e.g. G_ALTITUDE from basinObject
#' @param basin_object basinObject itself that belongs to input
#' (needed to create raster)
#' @param spatial_reference if set to TRUE raster is returned,
#' if set to FALSE matrix is returned
#' @return raster or matrix with specified information
#' (2D information of Basin)
#' @export
basin.create_raster <- function(input, basin_object, spatial_reference = TRUE) {

  if (is.list(basin_object)) {
    row_info <- basin_object$GR
    col_info <- basin_object$GC
    if (isTRUE(spatial_reference)) {
      warning("basinObject is list so spatialReference cannot be calculated \n
              raster without spatial reference is returned!")
      spatial_reference <- FALSE
    }
  } else {
    row_info <- basin_object@GR
    col_info <- basin_object@GC
  }

  array_size <- length(col_info)
  arr <- matrix(NA, nrow = max(row_info) - min(row_info) + 1,
                ncol = max(col_info) - min(col_info) + 1)
  col_info_tmp <- col_info - min(GC) + 1
  row_info_tmp <- row_info - min(row_info) + 1
  for (i in 1:array_size){
    arr[row_info_tmp[i], col_info_tmp[i]] <- input[i]
  }

  if (spatial_reference == TRUE) {

    cor_col <- basin_object@cont@corCol
    cor_row <- basin_object@cont@corRow

    #TODO: maybe here is a minor bug...that raster is shifted by one
    x_min <- (min(col_info) + cor_col - 1) / 12 - 180
    x_max <- (max(col_info) + cor_col - 1) / 12 - 180 + 1 / 12
    y_min <- -((max(row_info) + cor_row - 1) / 12 - 90) - 1 / 12
    y_max <- -((min(row_info) + cor_row - 1) / 12 - 90)
    crs_name <- raster::crs("+init=epsg:4326")
    arr <- raster::raster(arr, xmn = x_min, xmx = x_max,
                          ymn = y_min, ymx = y_max, crs = crs_name)
  } else {
    arr <- raster::raster(arr)
  }

  return(arr)
}

################################################################################
#' @title  create Average from vector or matrix of basin
#' (vector needs ng entries)
#' @description #Functions to get average values of basin
#' @param input input vector of basin e.g. SoilwaterContent from basinObject
#' @param basin_object basinObject itself that belongs to input
#' @param type as string (land or WB or all) to define which reference are
#' should be used to calculated weighted mean as basin wide average
#' (landfrac or 1-landfrac or 1) * GAREA
#' @return float or vector
#' @export
basin.create_average <- function(input, basin_object, type = "land") {

  landfrac <- basin_object@landfrac
  area <- basin_object@GAREA
  array_size <- length(area)

  #NULL if Type is not defined
  frac <- switch(type, "land" = landfrac, "WB" = 1 - landfrac, "all" = 1)

  if (is.vector(input) && length(input) == array_size) {
      #calculation of Volume for landinput (given in mm)
      volume <- sum(input * area * frac)
      #calculation of mm equivalent
      basin_mean <- volume / sum(area)

  } else if (is.matrix(input) && dim(input)[2] == array_size) {
    basin_mean <- sapply(1:dim(input)[1], function(x) {
                        sum(input[x, ] * area * frac) / sum(area)
                        }
                        )
  } else {
    stop("input has a undefined type - should be vector or matrix with length
         of array_size and in case of a matrix length of timeseries as nrows")
  }
  return(basin_mean)
}

################################################################################

#' @title basin.createWaterBalance
#' @description Function to create Waterbalance for basin
#' @param model_output output of the model (--> runModel())
#' @param climate_object climateObject which was used durign the simulation
#' @param basin_object basinObject itself that belongs to input
#' @param start2use Date to use as start, given as string in the format
#' "\%dd.\%mm.\%.YYYY"
#' @param end2use Date to use as end, given as string in the format
#' "\%dd.\%mm.\%.YYYY"
#' @return List of waterbalance elements in the unit mm
#' (referring to the total basin area)
#' @export
basin.createWaterBalance <- function(model_output, climate_object, basin_object,
                                      start2use = NULL, end2use = NULL) {

  #check Date format
  format <- "%d.%m.%Y"
  if ((is.na(as.Date(start2use, format = format))) ||  (is.na(as.Date(end2use, format = format)))) {
    stop("Error using start2use or end2use - Please make sure that dates are
          given as string in the format %d.%m.%y or as NULL")
  }

  #getting indexof Date to examine
  if (is.null(start2use)) {
     start2use <- climate_object@start
  } else {
    if (as.Date(start2use, format = format) < as.Date(climate_object@start, format = format)) {
    stop("start2use is not defined properly,
        choose date within simulation period")
    }
  }

  if (is.null(end2use)) {
    end2use <-  climate_object@end
  } else {
    if (as.Date(end2use, format = format) > as.Date(climate_object@end, format = format)) {
      stop("end2use is not defined properly,
      choose date wihtin simulaiton period")
    }
  }

  sim_time <- seq(as.Date(climate_object@start, format = format),
                  as.Date(climate_object@end, format = format), by = 1)
  n1 <- which(as.Date(start2use, format = format) == sim_time)
  n2 <- which(as.Date(end2use, format = format) == sim_time)


  #water use - Not sure if sign of water uses are correctly implemented!
  use_gw <- sum(model_output$daily$Fluxes$Flux_dailyWaterUseGW[n1:n2, ] *
                basin_object@landfrac * basin_object@GAREA)
  use_sw <- sum(model_output$routing$WaterUseSW[n1:n2, ] *
                basin_object@landfrac * basin_object@GAREA)

  if (use_gw + use_sw > 0) {
    stop("Please make sure that Uses are implemented with the correct sign -
          talk to Jenny Kupzig")
    }

  #landfraction
  prec_land <- sum(climate_object@prec[n1:n2, ] *
                  basin_object@landfrac * basin_object@GAREA)
  ET_land <- sum((model_output$daily$Fluxes$InterceptionEvapo[n1:n2, ] +
                    model_output$daily$Fluxes$Flux_Sublimation[n1:n2, ] +
                    model_output$daily$Fluxes$Flux_dailyAET[n1:n2, ]) *
                    basin_object@landfrac * basin_object@GAREA)
  PET_land <- sum(model_output$daily$Fluxes$PET[n1:n2, ] *
                  basin_object@landfrac * basin_object@GAREA)

  if (PET_land < ET_land) {
    warning("Potential Evapotranspiration is smaller than actual
              Evapotranspiration, please check model equation!")
  }
  s_start <- sum((model_output$daily$Storages$CanopyContent[n1, ] +
                    model_output$daily$Storages$SnowContent[n1, ] +
                    model_output$daily$Storages$SoilContent[n1, ] +
                    model_output$daily$Storages$GroundwaterContent[n1, ]) *
                    basin_object@landfrac * basin_object@GAREA)
  s_end   <- sum((model_output$daily$Storages$CanopyContent[n2, ] +
                    model_output$daily$Storages$SnowContent[n2, ] +
                    model_output$daily$Storages$SoilContent[n2, ] +
                    model_output$daily$Storages$GroundwaterContent[n2, ]) *
                    basin_object@landfrac * basin_object@GAREA)
  ds_land <- s_end - s_start

  #waterfraction
  prec_water <- sum(climate_object@prec[n1:n2, ] *
                    (basin_object@G_RESAREA + basin_object@G_LAKAREA) +
                     climate_object@prec[n1:n2, ] *
                     (basin_object@G_LOCLAK + basin_object@G_LOCWET +
                     basin_object@G_GLOWET) / 100 * basin_object@GAREA)

  ET_water <- sum(model_output$routing$locLake$Evapo[n1:n2, ] +
                    model_output$routing$locWetland$Evapo[n1:n2, ] +
                    model_output$routing$gloLake$Evapo[n1:n2, ] +
                    model_output$routing$gloWetland$Evapo[n1:n2, ] +
                    model_output$routing$Res$Evapo[n1:n2, ])
  s_start <- sum(model_output$routing$locLake$Storage[n1, ] +
                    model_output$routing$locWetland$Storage[n1, ] +
                    model_output$routing$gloLake$Storage[n1, ] +
                    model_output$routing$gloWetland$Storage[n1, ] +
                    model_output$routing$Res$Storage[n1, ] +
                    model_output$routing$River$RiverStorage[n1, ])
  s_end    <- sum(model_output$routing$locLake$Storage[n2, ] +
                     model_output$routing$locWetland$Storage[n2, ] +
                     model_output$routing$gloLake$Storage[n2, ] +
                     model_output$routing$gloWetland$Storage[n2, ] +
                     model_output$routing$Res$Storage[n2, ] +
                     model_output$routing$River$RiverStorage[n2, ])
  ds_water <- s_end - s_start

  #outflow of basin
  discharge <- sum(model_output$routing$River$Discharge[n1:n2]) *
                sum(basin_object@GAREA)

  # Test difference of Precipitation amount due to inaccuracy and discrepancy
  # 0n waterbodies (GLOLAK*GAREA ~ LAKAREA + RESAREA AND global/lokal
  # waterbodies in percent not promille)
  prec_total <- sum(climate_object@prec[n1:n2, ] * basin_object@GAREA)
  if (prec_total != prec_land + prec_water) {
      warning(sprintf("Discrepancy in total precipitation of %f mm due
              to waterbodies",
              (prec_total - prec_land - prec_water) / sum(basin_object@GAREA)))
  }

  # in mm*km2
  list2return <- list("Q" = discharge,
                      "P" = prec_land + prec_water,
                      "E" = ET_water + ET_land,
                      "dS" = ds_land + ds_water,
                      "Uses" = use_gw + use_sw)

   # mm
  list2return <- lapply(1:length(list2return), function(x) {
                        list2return[[x]] / sum(basin_object@GAREA)
                        }
                        )

  names(list2return) <- c("Q", "P", "E", "dS", "Uses")

  diff <- list2return$P -
          list2return$Q -
          list2return$E -
          list2return$dS -
          list2return$Uses
  if (diff != 0) {
    warning(sprintf("Waterbalance is not close, disrepancy of %f mm. \n
                    Note that this could be due to storage issues because
                    storage is always stored after simulation of day.",
                    (list2return$P -
                    list2return$Q -
                    list2return$E -
                    list2return$dS -
                    list2return$Uses) / sum(basin_object@GAREA)))
    }

  return(list2return)
}