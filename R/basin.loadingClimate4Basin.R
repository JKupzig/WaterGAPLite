#' @title Loading basinwide climate data
#' @description Function to read basinwide climate data
#' @param name name of variable to be read
#' @param grdc_number grdc number of basin that is used to save climate data in ClimateShortcut-folder
#' @param basin_index basinIndex to define which cells of continental raster belong to basin
#' @param data_dir filepath to data-folder - usually ...base/data
#' @param sim_period_date DateVector defining the simulation period
#' @return Matrix with climate data (nrow=days, ncol=cells of basin)
#' @importFrom utils read.table
#' 
basin.loading_climate_basin <- function(name = "prec", grdc_number, basin_index,
                                      data_dir, sim_period_date) {

  array_size <- length(basin_index)
  root <- file.path(data_dir, "climate", "basin",
                    paste0(as.character(grdc_number), "_", name, ".txt"))

  if (!file.exists(root)) {
    warning(paste("File:",
                  paste0(as.character(grdc_number), "_", name, ".txt"),
                  "does not exists in", file.path(data_dir, name, "basin"),
                  "--> NA-Matrix returned or introduced,
                  when climateShortCut is used")
                  )

    tmp <- matrix(data = NA, nrow = length(sim_period_date), ncol = array_size)
    rownames(tmp) <- as.character(sim_period_date)

  } else {
    tmp <- data.table::fread(root, skip = 2)
    tmp <- unlist(tmp, use.names = FALSE)
    tmp <- matrix(tmp, nrow = length(tmp) / array_size, ncol = array_size)

    start_date <- read.table(file = root, header = FALSE,
                          nrows = 1, stringsAsFactors = FALSE)
    ts <- seq(as.Date(start_date$V1, format = "%Y-%m-%d"),
          as.Date(start_date$V1, format = "%Y-%m-%d") + nrow(tmp) - 1,
          by = "day")

    id2use <- which(ts %in% sim_period_date)
    tmp <- tmp[id2use, ]

    rownames(tmp) <- as.character(ts[id2use])
  }

  return(tmp)
}
