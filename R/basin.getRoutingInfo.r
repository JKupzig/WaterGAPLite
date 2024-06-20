#' @title Loading Routing Information for Basin
#' @description Function to read routing information in routing folder and update basinObject
#' @param basin_object basinObject that needs to be updated with information
#' @return updated basinObject
#' @importFrom methods slot


basin.get_routing_info <- function(basin_object) {

  data_dir <- slot(basin_object, "cont")@DataDir
  trans_matrix <- basin_object@gcrcWithoutZero
  basin_index <- basin_object@basinIndex
  cont_name <- slot(basin_object, "cont")@contName

  river_length <- reading_unf("G_RIVER_LENGTH.UNF0", data_dir,
                                trans_matrix, basin_index,
                                name = "routing", cont = cont_name)
  river_slope <- reading_unf("G_RIVERSLOPE.UNF0", data_dir,
                                trans_matrix, basin_index,
                                name = "routing", cont = cont_name)
  river_roughness <- reading_unf("G_ROUGHNESS.UNF0", data_dir,
                                trans_matrix, basin_index,
                                name = "routing", cont = cont_name)

  #Needs to be created based on FlowDir!
  flow_acc <-reading_unf(sprintf("G_FLOW_ACC_%s.UNF4", cont_name),
                        #"G_FLOW_ACC.UNF2",
                        data_dir, trans_matrix,
                        basin_index, name="routing", cont=cont_name)
  #gcrc-ID in basin - sorted as all input files 
  outflow <-reading_unf("G_OUTFLC.UNF4", data_dir, trans_matrix,
                        basin_index, name="routing", cont=cont_name)

  #minimum and maximum flows for Schneider's algorithm
  min_flow <-reading_unf("G_7daymin.UNF0", data_dir, trans_matrix,
                        basin_index, name="routing", cont=cont_name)

  max_flow <-reading_unf("G_7daymax.UNF0", data_dir, trans_matrix,
                        basin_index, name="routing", cont=cont_name)

  route_order <- rep(NA, length(flow_acc))
  routing_steps <- unique(flow_acc)
  routing_steps <- routing_steps[order(routing_steps)]

  for (i in 1:length(routing_steps)){
     index <- which(flow_acc == routing_steps[i])
     route_order[index] <- i
  }

  # have to change gcrc-IDs in outflow 
  # so they correspondens with index in input files 
  outflow_new <- sapply(1:length(outflow), function(x)
                         which(outflow[x] == trans_matrix[basin_index])[1])

  #note that outlet has NA because downstream cell is not included in basin
  if (length(which(is.na(outflow_new))) == 1) {
    outflow_new[is.na(outflow_new)] <- -999
  } else {
    stop("There is an error when creating routing order - 
          have to check in detail!")
  }

  basin_object@G_riverLength <- river_length
  basin_object@G_riverSlope <- river_slope
  basin_object@G_riverRoughness <- river_roughness
  basin_object@routeOrder <- route_order
  basin_object@outflow <- outflow_new
  basin_object@G_7daymin <- min_flow
  basin_object@G_7daymax <- max_flow
  return(basin_object)

}
