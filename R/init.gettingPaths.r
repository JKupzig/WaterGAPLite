#' @title Path Definition
#' @description Function to determine paths in r
#' so package can find input and knows where to write the output
#' @param cont continent as string where basin is
#' part of (as, af, au, na, eu, sa)
#' @param base filepath to folder where
#' folder for data, source_code and output are located
#' @return named list with pathes to use
init.getting_paths <- function(cont, base = NULL) {

  if (is.null(base)) {
    stop("basePath not defined")
  }

  base_dir <- base
  data_dir <- file.path(base, "data")
  output_dir <- file.path(base, "output", cont)
  climate_shortcut <- file.path(data_dir, "climateShortCut", cont)
  system_values <- file.path(data_dir, "SystemValues", cont)


  return(list("BaseDir" = base_dir,
              "DataDir" = data_dir,
              "OutputDir" = output_dir,
              "climateShortcut" = climate_shortcut,
              "SystemValues" = system_values))
}
