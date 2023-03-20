#' @title Loading worldwide EWEMBI data
#' @description Function to read worldwide (EWEMBI) climate data for basin
#' @param name name of variable to be read
#' @param basin_index basinIndex to define which cells of continental raster belong to basin
#' @param trans_matrix transformation Matrix to transform global data to basinwide data (not sure if this is correct)
#' @param data_dir filepath to data-folder - usually ...base/data
#' @param wg2with5min_mask transformation information to get basinwide information from global met. data
#' @param watch_mask transformation information to get basinwide information from global met. data
#' @param sim_period_date DateVector defining the simulation period
#' @return Matrix with climate data (nrow=days, ncol=cells of basin)
basin.loading_climate_ewembi <- function(name = "prec", basin_index, trans_matrix,
                                         data_dir, wg2with5min_mask, watch_mask,
                                         sim_period_date) {

  array_size <- length(basin_index)
  #only climatedata for basin is read to save memory
  matrix_out <- matrix(data = NA, nrow = length(sim_period_date),
                      ncol = array_size)

  root <- file.path(data_dir, "climate", "global")

  start_year <- as.numeric(format(min(sim_period_date), "%Y"))
  end_year <- as.numeric(format(max(sim_period_date), "%Y"))

  start_month <- as.numeric(format(min(sim_period_date), "%m"))
  end_month <- as.numeric(format(max(sim_period_date), "%m"))

  months <- seq(1, 12, 1)
  years <- rep(start_year, 12)

  if (start_year == (end_year - 1)) {

    unique_years <- seq(start_year, end_year, 1)
    first_months <- seq(start_month, 12, 1)
    last_months <- seq(1, end_month, 1)

    months <- c(first_months, rep(1:12, length(unique_years) - 2), last_months)

    years = c(rep(unique_years[1], length(first_months)),
              rep(unique_years[length(unique_years)], length(last_months)))

  } else {
    unique_years <- seq(start_year, end_year, 1)
    first_months <- seq(start_month, 12, 1)
    last_months <- seq(1, end_month, 1)

    months <- c(first_months, rep(1:12, length(unique_years) - 2), last_months)

    years = c(rep(unique_years[1], length(first_months)),
              sort(rep(unique_years[2]:unique_years[length(unique_years)-1], 12)),
              rep(unique_years[length(unique_years)], length(last_months)))
  }

  file2read <- sprintf("GPREC_%d_%d.31.UNF0", years[1], months[1])
  storage_size <- file.info(file.path(root, file2read))$size / (4 * 31)

  mask_info <- wg2with5min_mask #by default
  if (storage_size == 67420) {
    mask_info <- watch_mask
  }

  if (name == "prec") {
    filename <- "GPREC_%d_%d.31.UNF0"
  } else if (name == "shortwave") {
    filename <- "GSHORTWAVE_%d_%d.31.UNF0"
  } else if (name == "longwave") {
    filename <- "GLONGWAVE_DOWN_%d_%d.31.UNF0"
  } else if (name == "temp") {
    filename <- "GTEMP_%d_%d.31.UNF0"
  }

  for (i in 1:length(months)) {

    month <- months[i]
    year <- years[i]
    file <- sprintf(filename, year, month)

    climate_data <- reading_unf(file, data_dir=data_dir,
                                trans_matrix = NULL, basin_index = NULL,
                                name = "climate", type_climate = 1)

    
    filepath <- file.path(root, file)
    nlayer_climate <- get_info_file(filepath)[[1]]

    index_year <- which(as.numeric(format(sim_period_date, "%Y")) == year)
    index_month <- which(as.numeric(format(sim_period_date[index_year], "%m")) == month)
    row_matrix <- index_year[index_month]

    ###separating layers from climate data
    for (j in 1:length(row_matrix)){
      climate_data_day <- sapply(1:(length(climate_data)/nlayer_climate),
                                function(x) climate_data[(nlayer_climate * (x - 1)) + j])
      matrix_out[row_matrix[j], ] <- climate_data_day[mask_info]
    }
  }

  rownames(matrix_out) <- as.character(sim_period_date)
  return(matrix_out)

}
