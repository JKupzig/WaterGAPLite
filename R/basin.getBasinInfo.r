#' @title Loading basin information from UNF-Files 
#' @description Function that loads info from files into basinObject
#' @param basinObject for basin that needs to be updated
#' @return updated basinObject 
#' @importFrom methods slot
#' @importFrom methods slot<-
#' 
basin.getBasinInfo <- function(basinObject){
  
  transMatrix <- basinObject@gcrcWithoutZero
  basIndex <- basinObject@basinIndex
  DataDir <- slot(basinObject, "cont")@DataDir
  ngLand <- slot(basinObject, "cont")@ng_land
  contName <- slot(basinObject, "cont")@contName
  
  folder <- file.path(DataDir, "basinInfo", contName)
  filesUNF <- getFileList(Type="basinInfo")
  
  for (f in filesUNF) {
    vec <- readingUNF(f, DataDir, transMatrix, basIndex, ng_land=ngLand, cont=contName)
    VarName <- unlist(strsplit(f, '.UNF'))[1]
    slot(basinObject, VarName) <- vec
  }
  
  folder <- file.path(DataDir, "changingFiles", contName)
  filesUNF <- getFileList(Type="changingFiles") 
  for (f in filesUNF) {
    vec <- readingUNF(f,DataDir,transMatrix, basIndex, name="changingFiles", cont=contName)
    VarName <- unlist(strsplit(f, '.UNF'))[1]
    slot(basinObject, VarName) <- vec
  }
  
  return(basinObject)
}

