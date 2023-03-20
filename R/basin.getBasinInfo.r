#' @title Loading basin information from UNF-Files
#' @description Function that loads info from files into basinObject
#' @param basin_object for basin that needs to be updated
#' @return updated basinObject
#' @importFrom methods slot
#' @importFrom methods slot<-
basin.get_basin_info <- function(basin_object) {

  ENDING <- ".UNF"

  trans_matrix <- basin_object@gcrcWithoutZero
  basin_index <- basin_object@basinIndex
  data_dir <- slot(basin_object, "cont")@DataDir
  ng_land <- slot(basin_object, "cont")@ng_land
  cont_name <- slot(basin_object, "cont")@contName

  files_unf <- get_file_list(type = "basinInfo")
  for (file in files_unf) {
    vec <- reading_unf(file, data_dir, trans_matrix, basin_index,
                        ng_land = ng_land, cont = cont_name)
    var_name <- unlist(strsplit(file, ENDING))[1]
    slot(basin_object, var_name) <- vec
  }

  files_unf <- get_file_list(type = "changingFiles")
  for (f in files_unf) {
    vec <- reading_unf(f, data_dir, trans_matrix, basin_index,
                        name = "changingFiles", cont = cont_name)
    VarName <- unlist(strsplit(f, ENDING))[1]
    slot(basin_object, VarName) <- vec
  }

  return(basin_object)
}
