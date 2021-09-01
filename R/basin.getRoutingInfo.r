#' @title Loading Routing Information for Basin
#' @description Function to read routing information in routing folder and update basinObject
#' @param basinObject basinObject that needs to be updated with information
#' @return updated basinObject
#' @importFrom methods slot


basin.getRoutingInfo <- function(basinObject){
  
  DataDir <- slot(basinObject, "cont")@DataDir
  transMatrix <- basinObject@gcrcWithoutZero
  basIndex <- basinObject@basinIndex
  contName <- slot(basinObject, "cont")@contName
  
  G_riverLength <- readingUNF("G_RIVER_LENGTH.UNF0", DataDir, transMatrix, basIndex, name="routing", cont=contName)
  G_riverSlope <- readingUNF("G_RIVERSLOPE.UNF0", DataDir, transMatrix, basIndex, name="routing", cont=contName)
  G_riverRoughness <- readingUNF("G_ROUGHNESS.UNF0", DataDir, transMatrix, basIndex, name="routing", cont=contName)
  
  #Needs to be created based on FlowDir!
  flowAcc <-readingUNF("G_FLOW_ACC.UNF4", DataDir, transMatrix, basIndex, name="routing", cont=contName)
  outflow <-readingUNF("G_OUTFLC.UNF4", DataDir, transMatrix, basIndex, name="routing", cont=contName) #gcrc-ID in basin - sorted as all input files 
  
  routeOrder = rep(NA, length(flowAcc))
  routingSteps <- unique(flowAcc)
  routingSteps <- routingSteps[order(routingSteps)]
  #routeOrder <- order(flowAcc) #das funktioniert nicht, da gleiche Zahlen einfach trotzdemm sortiwert werden
  for (i in 1:length(routingSteps)){
     index <- which(flowAcc == routingSteps[i])
     routeOrder[index] <- i
  }
  #have to change gcrc-IDs in outflow so they correspondens with index in input files 
  outflow_new <- sapply(1:length(outflow), function(x) which(outflow[x] == transMatrix[basIndex])[1])
  #note that outlet has NA because downstream cell is not included in basin 
  if (length(which(is.na(outflow_new))) == 1){
    outflow_new[is.na(outflow_new)] = -999;
  } else {
    stop("There is an error when creating routing order - have to check in detail!")
  }

  basinObject@G_riverLength <- G_riverLength
  basinObject@G_riverSlope <- G_riverSlope
  basinObject@G_riverRoughness <- G_riverRoughness
  basinObject@routeOrder <- routeOrder
  basinObject@outflow <- outflow_new
  
  return(basinObject)
  
}
