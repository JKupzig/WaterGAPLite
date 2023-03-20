#' @title Defining Files to be read into model
#' @description Function to get FileList to read
#' @param type that defines which information is required
#' (basinInfo, changingFiles or routing)
#' @return FileList with files that needs to be read

get_file_list <- function(type = "basinInfo") {

  file_list <- NULL
  if (type == "basinInfo") {
    file_list <- c("G_ALTITUDE.UNF2",
                  "G_AQ_FACTOR.UNF1",
                  "G_ARID_HUMID.UNF2",
                  "G_BATJES.UNF2",
                  "G_ELEV_RANGE.26.UNF2",
                  "G_GAMMA_HBV.UNF0",
                  "G_GLOLAK.UNF1",
                  "G_GLOWET.UNF1",
                  "G_LAKAREA.UNF4",
                  "G_LOCLAK.UNF1",
                  "G_LOCWET.UNF1",
                  "G_PERMAGLAC.UNF1",
                  "G_RES_TYPE.UNF1",
                  "G_RESAREA.UNF4",
                  "G_SLOPE_CLASS.UNF1",
                  "G_STORAGE_CAPACITY.UNF0",
                  "G_TEXTURE.UNF1",
                  "G_WG3_WATCH.UNF4",
                  "G_WG3_WG2WITH5MIN.UNF4",
                  "GAREA.UNF0",
                  "GBUILTUP.UNF0",
                  "GLCT.UNF1",
                  "GC.UNF2",
                  "GCRC.UNF4",
                  "GR.UNF2")

  } else if (type == "changingFiles") {
    file_list <- c("G_ALLOC_COEFF.20.UNF0",
                  "G_BANKFULL.UNF0",
                  "G_MEAN_INFLOW.12.UNF0",
                  "G_MEAN_INFLOW.UNF0",
                  "G_START_MONTH.UNF1")

  } else if (type == "routing") {
    file_list <- c("G_FLOW_ACC.UNF4",
                  "G_OUTFLC.UNF4",
                  "G_RIVER_LENGTH.UNF0",
                  "G_RIVERSLOPE.UNF0",
                  "G_ROUGHNESS.UNF0")
  }

  return(file_list)

}