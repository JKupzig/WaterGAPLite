#' @title Getting File Information of UNF-File
#' @description gets File info how to decode binaries
#' @param filepath path to file 
#' @return list(nlayers, Size, Type) 

getInfoFile <- function(filepath){
  
  name <- strsplit(filepath, "/")[[1]][length(strsplit(filepath, "/")[[1]])]
  layers <- strsplit(name, "[.]")[[1]][2]
  
  if (startsWith(layers, "U")){
    nlayers = 1
  } else {
    nlayers = as.integer(layers)
  }
  
  Code <- as.integer(substr(name, nchar(name), nchar(name)))
  if (Code == 0){
    Size = 4
  } else if (Code == 1){
    Size = 1
  } else if (Code == 2){
    Size = 2
  } else if (Code == 4){
    Size = 4
  }
  Type <- substr(name,  nchar(name)-3, nchar(name))
  
  return(list(nlayers, Size, Type))
}