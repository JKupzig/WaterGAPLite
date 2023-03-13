#' @title Getting File Information of UNF-File
#' @description gets File info how to decode binaries
#' @param filepath path to file
#' @return list(nlayers, Size, Type)

get_info_file <- function(filepath) {

  name <- strsplit(filepath, "/")[[1]][length(strsplit(filepath, "/")[[1]])]
  layers <- strsplit(name, "[.]")[[1]][2]

  if (startsWith(layers, "U")) {
    nlayers <- 1
  } else {
    nlayers <- as.integer(layers)
  }

  code <- as.integer(substr(name, nchar(name), nchar(name)))
  if (code == 0) {
    size <- 4
  } else if (code == 1) {
    size <- 1
  } else if (code == 2) {
    size <- 2
  } else if (code == 4) {
    size <- 4
  }
  type <- substr(name,  nchar(name) - 3, nchar(name))

  return(list(nlayers, size, type))
}