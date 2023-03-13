#' @title Definition of Neighbouring Cells
#' @description Function to determine neighbouringh cells which is a pre-requesite for routeorder
#' @param basin_object basinObject that needs to be updated by informaiton of neighbouring cells
#' @return updated basinObject

basin.get_neighbourcells <- function(basin_object) {

  array_size <- basin_object@array_size
  gcrc <- basin_object@GCRC
  col_location <- basin_object@GC
  row_location <- basin_object@GR
  #using GC and GR to find neighboruign cells
  neighbour_cells <- matrix(NA, nrow = 8, ncol = array_size)
  # RowOrder:
  # 6 7 8
  # 4 - 5
  # 1 2 3

  for (i in 1:array_size){
    gcrc_id <- gcrc[i]
    col_i <- col_location[i]
    row_i <- row_location[i]

    #finding positions
    id_1 <- which((col_location == col_i + 1) & (row_location == row_i - 0))
    id_2 <- which((col_location == col_i + 1) & (row_location == row_i - 1))
    id_3 <- which((col_location == col_i + 0) & (row_location == row_i - 1))
    id_4 <- which((col_location == col_i - 1) & (row_location == row_i - 1))
    id_5 <- which((col_location == col_i - 1) & (row_location == row_i - 0))
    id_6 <- which((col_location == col_i - 1) & (row_location == row_i + 1))
    id_7 <- which((col_location == col_i - 0) & (row_location == row_i + 1))
    id_8 <- which((col_location == col_i + 1) & (row_location == row_i + 1))

    neighbour_cells[1, i] <- ifelse(length(id_1) == 0, 0, id_1)
    neighbour_cells[2, i] <- ifelse(length(id_2) == 0, 0, id_2)
    neighbour_cells[3, i] <- ifelse(length(id_3) == 0, 0, id_3)
    neighbour_cells[4, i] <- ifelse(length(id_4) == 0, 0, id_4)
    neighbour_cells[5, i] <- ifelse(length(id_5) == 0, 0, id_5)
    neighbour_cells[6, i] <- ifelse(length(id_6) == 0, 0, id_6)
    neighbour_cells[7, i] <- ifelse(length(id_7) == 0, 0, id_7)
    neighbour_cells[8, i] <- ifelse(length(id_8) == 0, 0, id_8)
  }

  basin_object@NeighbouringCells <- neighbour_cells
  return(basin_object)

}