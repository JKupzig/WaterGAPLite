#' @title Definition of Basin
#' @description Function initiliazes basin (basin shape is defined and global information to build basin raster is saved)
#' @param basin_object basinObject that needs to be updated with information
#' @return changed basinObject
#' @importFrom methods slot

basin.def_basin <- function(basin_object) {

  lat <- slot(basin_object, "location")[2]
  long <- slot(basin_object, "location")[1]

  cor_row <- slot(basin_object, "cont")@corRow
  cor_col <- slot(basin_object, "cont")@corCol
  ncol <- slot(basin_object, "cont")@ncol
  data_dir <- slot(basin_object, "cont")@DataDir
  cont_name <- slot(basin_object, "cont")@contName

  col <- floor((long + 180) * 12 + 1 - cor_col)
  row <- floor((-lat + 90) * 12 + 1 - cor_row)
  index_on_grid_with_ocean <- (row - 1) * ncol + col

  to.read <- file(file.path(data_dir, "routing",
                  cont_name, "G_OUTFLC.UNF4"), "rb")
  outflow <- readBin(to.read,what=integer(),
                      endian = "big", size = 4, n = 99999999)
  close(to.read)

  to.read <- file(file.path(data_dir, "basinInfo",
                  cont_name, "GCRC.UNF4"), "rb")
  gcrc <- readBin(to.read, what = integer(),
                  endian = "big", size = 4, n = 99999999)
  close(to.read)
  gcrc_id_outlet <- gcrc[index_on_grid_with_ocean]
  gcrc_without_ocean <- gcrc[gcrc != 0]

  #necessary because of lakes in GCRC that have high ID's
  outflow <- outflow[gcrc_without_ocean]
  basin <- WaterGAPLite::tools_DefDrainageCells(gcrc_id_outlet, gcrc_without_ocean, outflow)
  #index position - so just 1:n without special emphasis to lakes
  basinIndex <- which(gcrc_without_ocean %in% basin)

  basin_object@gcrcWithoutZero <- gcrc_without_ocean
  basin_object@array_size <- length(basinIndex)
  basin_object@basinIndex <- basinIndex

  return(basin_object)
}