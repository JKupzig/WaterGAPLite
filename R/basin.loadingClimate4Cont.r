#' @title Loading climate from continental data
#' @description Function to read continental climate infromation
#' @param name name name of variable to be read
#' @param basin_index basinIndex to define which cells of continental raster belong to basin
#' @param trans_matrix transformation Matrix to transform global data to basinwide data (not sure if this is correct)
#' @param data_dir filepath to data-folder - usually ...base/data
#' @param sim_period_date DateVector defining the simulation period
#' @param cont continent as string (au, af, as, eu, na, sa)
#' @return Matrix with climate data (nrow=days, ncol=cells of basin)

basin.loading_climate_continental <- function(name = "prec", basin_index,
                                              trans_matrix, data_dir,
                                              sim_period_date, cont) {


  array_size <- length(basin_index)
  matrix_out <- matrix(data = NA, nrow = length(sim_period_date),
                       ncol = array_size)

  start_year <- as.numeric(format(min(sim_period_date), "%Y"))
  end_year <- as.numeric(format(max(sim_period_date), "%Y"))

  start_month <- as.numeric(format(min(sim_period_date), "%m"))
  end_month <- as.numeric(format(max(sim_period_date), "%m"))

  #special condition, when only one year needs to be read in
  months <- seq(1, 12, 1)
  years <- rep(start_year, 12)

  if (start_year == (end_year - 1)) {

    unique_years <- seq(start_year, end_year, 1)
    first_months <- seq(start_month, 12, 1)
    last_months <- seq(1, end_month, 1)

    months <- c(first_months,
                rep(1:12, length(unique_years)-2),
                last_months)

    years <- c(rep(unique_years[1], length(first_months)),
               rep(unique_years[length(unique_years)], length(last_months)))

  } else {
    unique_years <- seq(start_year, end_year, 1)
    first_months <- seq(start_month, 12, 1)
    last_months <- seq(1, end_month, 1)

    months <- c(first_months, rep(1:12, length(unique_years) - 2), last_months)

    years <- c(rep(unique_years[1], length(first_months)),
              sort(rep(unique_years[2]:unique_years[length(unique_years) - 1], 12)),
              rep(unique_years[length(unique_years)], length(last_months)))
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

    #check if file exists, if not, return NA Matrix and throw warning
    filepath <- file.path(data_dir, "climate", "continental", cont, file)
    if (!file.exists(filepath)){
      warning(paste("File:", file,
                  "does not exists in",
                  file.path(data_dir, name, "continental", cont),
                  "--> NA-Matrix returned or introduced,
                  when climateShortCut is used"))

      matrix_out <- matrix(data = NA, nrow = length(sim_period_date),
                            ncol = array_size)
      break
    } else {
      climate_data <- reading_unf(file2read = file, data_dir = data_dir,
                                  trans_matrix = trans_matrix, 
                                  basin_index = basin_index,
                                  name = "climate", type_climate = 2, 
                                  cont = cont)

      index_year <- which(as.numeric(format(sim_period_date, "%Y")) == year)
      index_month <- which(as.numeric(
                          format(sim_period_date[index_year], "%m")) == month)
      row_matrix <- index_year[index_month]
      matrix_out[row_matrix, ] <- climate_data[1:length(row_matrix), ]
    }
  }
  rownames(matrix_out) <- as.character(sim_period_date)
  return(matrix_out)
}
