#' @title Path Definition
#' @description Function to determine paths in r so package can find input and knows where to write the output
#' @param cont continent as string where basin is part of (as, af, au, na, eu, sa)
#' @param base filepath to folder where folder for data, source_code and output are located
#' @return named list with pathes to use


init.gettingPaths <- function(cont, base=NULL){
  
  #rstudio own function to determine baseDir based on location of excecued script
  if (is.null(base)){
    stop("basePath not defined")
    #file_dir <- rstudioapi::getSourceEditorContext()$path
    #base <- dirname(dirname(dirname(file_dir)))
  }
  
  #print(base)
  BaseDir <- base
  DataDir <- file.path(base, "data")
  OutputDir <- file.path(base, "output", cont)
  climateShortcut <- file.path(DataDir, "climateShortCut", cont)
  SystemValues <- file.path(DataDir, "SystemValues", cont)

  
  return(list("BaseDir"=BaseDir, "DataDir"=DataDir, "OutputDir"=OutputDir, "climateShortcut"=climateShortcut, "SystemValues"=SystemValues))
}
