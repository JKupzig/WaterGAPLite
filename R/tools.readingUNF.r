#' @title  Reading UNF-File
#' @description function to read data from UNF-File
#' @param file2read filename to read
#' @param DataDir defined (global) direction for data - usually workindgirectory/data/
#' @param transMatrix usually gcrc information
#' @param basIndex vector to use to define basin
#' @param name name folder to use for reading
#' @param ng_land ng_land information, needed to read G_ELEV_RANGE.UNF
#' @param TypeClimate value to determine which type needs to be written: 0: normal layer, 1:global climate data 2:continental climate data
#' @param cont character to determine continent (af,au,as,eu,na,sa)
#' @return vector (single layer) oder matrix (mutlple layer) with each column is information for one cell in defined basin

#Note: ELEV_RANGE.26 (ng_land) and GAREA (nrow) Mean_Inflow.12 (multiple layers) have different construction!
#source("_getInfoFile.r")

readingUNF <- function(file2read, DataDir, transMatrix=NULL, basIndex, name="basinInfo", ng_land=NULL, TypeClimate=0, cont=NULL) {
  #ng_land only used to read ELEV_range
  ng <- length(transMatrix)

  if (TypeClimate == 1) {

    filepath <- file.path(DataDir, name, "global", file2read)
    filetype <- substr(file2read,  nchar(file2read)-3, nchar(file2read))
    nlayers  <- 31

    to.read = file(filepath, "rb")
    if (filetype == "UNF0") {
      Vals <- readBin(to.read,what=numeric(), endian = "big", size=4, n=99999999)
    } else if (filetype ==  "UNF1"){ #wrong Info from Ellen not character but integer
      Vals <- readBin(to.read,what=integer(), endian = "big", size=1, n=99999999)
    } else if (filetype ==  "UUF2"){
      Vals <- readBin(to.read,what=integer(), endian = "big", size=2, n=99999999, signed = F)
    } else if (filetype ==  "UNF2"){
      Vals <- readBin(to.read,what=integer(), endian = "big", size=2, n=99999999)
    } else if (filetype ==  "UNF4"){
      Vals <- readBin(to.read,what=integer(), endian = "big", size=4, n=99999999)
    }

    close(to.read)

  } else if (TypeClimate == 2) {

    filepath <- file.path(DataDir, name, "continental", cont, file2read)
    filetype <- substr(file2read,  nchar(file2read)-3, nchar(file2read))
    nlayers  <- 31
    to.read = file(filepath, "rb")
    if (filetype == "UNF0") {
      Vals <- readBin(to.read,what=numeric(), endian = "big", size=4, n=99999999)
    } else if (filetype ==  "UNF1"){ #wrong Info from Ellen not character but integer
      Vals <- readBin(to.read,what=integer(), endian = "big", size=1, n=99999999)
    } else if (filetype ==  "UUF2"){
      Vals <- readBin(to.read,what=integer(), endian = "big", size=2, n=99999999, signed = F)
    } else if (filetype ==  "UNF2"){
      Vals <- readBin(to.read,what=integer(), endian = "big", size=2, n=99999999)
    } else if (filetype ==  "UNF4"){
      Vals <- readBin(to.read,what=integer(), endian = "big", size=4, n=99999999)
    }
    close(to.read)
    #same es for g_mean_inflow and so on
    mat = matrix(NA, nlayers,length(Vals)/nlayers)
    for (j in 1:nlayers){
      mat[j,] = sapply(1:(length(Vals)/nlayers), function(x) Vals[(x-1)*nlayers+j])
      mat[j,] =  mat[j,][transMatrix] #have to check if transformation is needed!
    }
    Vals = mat

    if (!missing(basIndex)) {
      NewMat <- matrix(NA, nlayers, length(basIndex))
      for (k in 1:nlayers){ NewMat[k,] <- mat[k,][basIndex] } #only basinInfo is read into system so save memory
      Vals = NewMat
    }

  } else {
    filepath <- file.path(DataDir, name, cont, file2read)
    filetype <- substr(file2read,  nchar(file2read)-3, nchar(file2read))
    nlayers = getInfoFile(filepath)[[1]][1]

    to.read = file(filepath, "rb")
    if (filetype == "UNF0") {
      Vals <- readBin(to.read,what=numeric(), endian = "big", size=4, n=99999999)
    } else if (filetype ==  "UNF1"){ #wrong Info from Ellen not character but integer
      Vals <- readBin(to.read,what=integer(), endian = "big", size=1, n=99999999)
    } else if (filetype ==  "UUF2"){
      Vals <- readBin(to.read,what=integer(), endian = "big", size=2, n=99999999, signed = F)
    } else if (filetype ==  "UNF2"){
      Vals <- readBin(to.read,what=integer(), endian = "big", size=2, n=99999999)
    } else if (filetype ==  "UNF4"){
      Vals <- readBin(to.read,what=integer(), endian = "big", size=4, n=99999999)
    }

    close(to.read)
    #check if there are multilayers, if yes: create matrix
    #could be written in cpp to improve performance
    if (file2read == "GAREA.UNF0"){

      to.read = file(file.path(DataDir, name, cont, "GR.UNF2"), "rb")
      HelpInfo <- readBin(to.read,what=integer(), endian = "big", size=2, n=99999999)
      close(to.read)
      area <- Vals[HelpInfo]
      Vals <- area
      if (!missing(basIndex)) { Vals <- Vals[basIndex] } #only basinInfo is read into system so save memory

    } else if ((file2read != "G_ELEV_RANGE.26.UNF2") & (nlayers > 1)) {

      mat = matrix(NA, nlayers, length(Vals)/nlayers)
      for (j in 1:nlayers){
        mat[j,] = sapply(1:(length(Vals)/nlayers), function(x) Vals[(x-1)*nlayers+j])
        mat[j,] =  mat[j,][transMatrix] #have to check if transformation is needed!
      }
      Vals = mat

      if (!missing(basIndex)) {
        NewMat <- matrix(NA, nlayers, length(basIndex))
        for (k in 1:nlayers){ NewMat[k,] <- mat[k,][basIndex] } #only basinInfo is read into system so save memory
        Vals = NewMat
      }

    } else if (file2read == "G_ELEV_RANGE.26.UNF2") {

      mat = matrix(NA, nlayers,ng)
      #sepcial case because all cells with gcrc-ID > ng_land are not considered here
      for (j in 1:nlayers){
        hilf = sapply(1:ng_land, function(x) Vals[(x-1)*nlayers+j]) #there is an error using this formular!
        hilf = c(hilf, rep(NA, ng-ng_land))
        mat[j,] = hilf[transMatrix]
      }
      Vals = mat

      if (!missing(basIndex)) {
        NewMat <- matrix(NA, nlayers, length(basIndex))
        for (k in 1:nlayers){ NewMat[k,] <- mat[k,][basIndex] } #only basinInfo is read into system so save memory
        Vals = NewMat }

    } else if (file2read == "GCRC.UNF4"){

      Vals <- Vals[Vals > 0]
      if (!missing(basIndex)) { Vals <- Vals[basIndex] }

    } else {
      Vals <- Vals[transMatrix]
      #have to sort like gcrc because basIndex is based on gcrc ID
      if (!missing(basIndex)) { Vals <- Vals[basIndex] } #only basinInfo is read into system so save memory
    }
  }

  return(Vals)

}
