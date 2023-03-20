#' @title  Reading UNF-File
#' @description function to read data from UNF-File
#' @param file2read filename to read
#' @param data_dir defined (global) direction for data -
#' usually workindgirectory/data/
#' @param trans_matrix usually gcrc information
#' @param basin_index vector to use to define basin
#' @param name name folder to use for reading
#' @param ng_land ng_land information, needed to read G_ELEV_RANGE.UNF
#' @param type_climate value to determine which type needs to be written:
#' 0: normal layer, 1:global climate data 2:continental climate data
#' @param cont character to determine continent (af,au,as,eu,na,sa)
#' @return vector (single layer) oder matrix (mutlple layer) with each column
#' is information for one cell in defined basin

# Note: ELEV_RANGE.26 (ng_land) and GAREA (nrow) Mean_Inflow.12
# (multiple layers) have different construction!

reading_unf <- function(file2read, data_dir, trans_matrix = NULL,
                        basin_index, name = "basinInfo", ng_land = NULL,
                        type_climate = 0, cont = NULL) {

  #ng_land only used to read ELEV_range
  ng <- length(trans_matrix)

  if (type_climate == 1) {

    filepath <- file.path(data_dir, name, "global", file2read)
    nlayers  <- 31

    vals <- read_unf(filepath)


  } else if (type_climate == 2) {

    filepath <- file.path(data_dir, name, "continental", cont, file2read)
    nlayers  <- 31

    vals <- read_unf(filepath)

    #same es for g_mean_inflow and so on
    mat <- matrix(NA, nlayers, length(vals) / nlayers)
    for (row in 1:nlayers) {
      mat[row, ] <- sapply(1:(length(vals) / nlayers),
                        function(x) {
                          vals[(x - 1) * nlayers + row]
                          }
                        )
      mat[row, ] <-  mat[row, ][trans_matrix]
    }
    vals <- mat

    if (!missing(basin_index)) {
      new_mat <- matrix(NA, nlayers, length(basin_index))
      for (k in 1:nlayers) {
        new_mat[k, ] <- mat[k, ][basin_index]
      }
      vals <- new_mat
    }

  } else {

    filepath <- file.path(data_dir, name, cont, file2read)
    nlayers <- get_info_file(filepath)[[1]][1]

    vals <- read_unf(filepath)

    if (file2read == "GAREA.UNF0") {

      help_info <- read_unf(file.path(data_dir, name, cont, "GR.UNF2"))
      area <- vals[help_info]
      vals <- area
      if (!missing(basin_index)) {
        vals <- vals[basin_index] #only basinInfo is stored so save RAM
      }

    } else if ((file2read != "G_ELEV_RANGE.26.UNF2") && (nlayers > 1)) {

      mat <- matrix(NA, nlayers, length(vals) / nlayers)
      for (row in 1:nlayers){
        mat[row, ] <- sapply(1:(length(vals) / nlayers),
                    function(x) vals[(x - 1) * nlayers + row])
        mat[row, ] <- mat[row, ][trans_matrix]
      }
        vals <- mat

      if (!missing(basin_index)) {
        new_mat <- matrix(NA, nlayers, length(basin_index))
        for (row in 1:nlayers) {
          new_mat[row, ] <- mat[row, ][basin_index] #only basinInfo is stored
        }
        vals <- new_mat
      }

    } else if (file2read == "G_ELEV_RANGE.26.UNF2") {
      #special case because all cells with gcrc-ID > ng_land are not considered

      mat <- matrix(NA, nlayers, ng)

      for (j in 1:nlayers) {
        hilf <- sapply(1:ng_land, function(x) vals[(x - 1) * nlayers + j])
        hilf <- c(hilf, rep(NA, ng - ng_land))
        mat[j, ] <- hilf[trans_matrix]
      }
      vals <- mat

      if (!missing(basin_index)) {
        new_mat <- matrix(NA, nlayers, length(basin_index))
        for (k in 1:nlayers) {
          new_mat[k, ] <- mat[k, ][basin_index] #only basinInfo is stored
        }
        vals <- new_mat
      }

    } else if (file2read == "GCRC.UNF4") {

      vals <- vals[vals > 0]
      if (!missing(basin_index)) {
        vals <- vals[basin_index]
      }

    } else {
      vals <- vals[trans_matrix]
      #have to sort like gcrc because basIndex is based on gcrc ID
      if (!missing(basin_index)) {
        vals <- vals[basin_index] #only basinInfo is stored
      }
    }
  }

  return(vals)

  }

  ##############

  read_unf <- function(filepath) {

    file2read <- basename(filepath)
    filetype <- substr(file2read,  nchar(file2read) - 3, nchar(file2read))
    to_read <- file(filepath, "rb")
    if (filetype == "UNF0") {
      vals <- readBin(to_read, what = numeric(), endian = "big",
                      size = 4, n = 99999999)
    } else if (filetype ==  "UNF1") {
      vals <- readBin(to_read, what = integer(), endian = "big",
                      size = 1, n = 99999999)
    } else if (filetype ==  "UUF2") {
      vals <- readBin(to_read, what = integer(), endian = "big",
                      size = 2, n = 99999999, signed = FALSE)
    } else if (filetype ==  "UNF2") {
      vals <- readBin(to_read, what = integer(), endian = "big",
                      size = 2, n = 99999999)
    } else if (filetype ==  "UNF4") {
      vals <- readBin(to_read, what = integer(), endian = "big",
                      size = 4, n = 99999999)
    }

    close(to_read)

    return(vals)
  }
