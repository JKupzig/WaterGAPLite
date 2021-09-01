#' @title Definition of Basin
#' @description Function initiliazes basin (basin shape is defined and global information to build basin raster is saved)
#' @param basinObject basinObject that needs to be updated with information
#' @return changed basinObject
#' @importFrom methods slot
#' 
basin.defBasin <- function(basinObject){
  
  lat = slot(basinObject, "location")[2]
  long = slot(basinObject, "location")[1]
  
  corRow <- slot(basinObject, "cont")@corRow 
  corCol <- slot(basinObject, "cont")@corCol
  ncol <- slot(basinObject, "cont")@ncol
  DataDir <- slot(basinObject, "cont")@DataDir
  contName <- slot(basinObject, "cont")@contName

  Column = floor((long+180)*12+1-corCol)
  Row = floor((-lat+90)*12+1-corRow)
  index = (Row-1)*ncol + Column #on gird WITH ocean
  
  #print(file.path(DataDir, "routing", contName, "G_OUTFLC.UNF4"))
  to.read = file(file.path(DataDir, "routing", contName, "G_OUTFLC.UNF4"), "rb")
  outflow <- readBin(to.read,what=integer(), endian = "big", size=4, n=99999999)
  close(to.read)
  
  to.read = file(file.path(DataDir, "basinInfo", contName, "GCRC.UNF4"), "rb")
  gcrc <- readBin(to.read,what=integer(), endian = "big", size=4, n=99999999)
  close(to.read)
  outlet <- gcrc[index] #gcrc-ID
  gcrcWithoutZero <- gcrc[gcrc != 0] #gcrc WITHOUT ocean
  
  #not sure if this is right
  outflow <- outflow[gcrcWithoutZero] #necessary because of lakes in GCRC that have high ID's
  basin <- WaterGAPLite::tools_DefDrainageCells(outlet, gcrcWithoutZero, outflow) #noch nicht alle--> perfomance steigerung möglich!
  basinIndex <- which(gcrcWithoutZero %in% basin) #index position - so just 1:n without special emphasis to lakes
  
  basinObject@gcrcWithoutZero <- gcrcWithoutZero
  basinObject@array_size <- length(basinIndex)
  basinObject@basinIndex <- basinIndex
  
  return(basinObject) #is gcrc ID!
}