#' @title  create Raster from vector of basin
#' @description Functions to deal with vectoried basin values in a nice and easy way
#' @param input input vector of basin e.g. G_ALTITUDE from basinObject
#' @param basinObject basinObject itself that belongs to input (needed to create raster)
#' @param spatialReference if set to TRUE raster is returned, if set to FALSE matrix is returned
#' @return raster or matrix with specified information (2D information of Basin) 
#' @export
basin.createRaster <- function(input, basinObject, spatialReference = T){
  
  if (is.list(basinObject)) {
    GR <- basinObject$GR
    GC <- basinObject$GC
    if (isTRUE(spatialReference)) {
      warning("basinObject is list so spatialReference cannot be calculated \n
              raster without spatial reference is returned!")
      spatialReference <- F
    }
  } else {
    GR <- basinObject@GR
    GC <- basinObject@GC
  }
  
  array_size <- length(GC)

  arr <- matrix(NA, nrow=max(GR)-min(GR)+1, ncol=max(GC)-min(GC)+1)
  GC_tmp <- GC - min(GC)+1
  GR_tmp <- GR - min(GR)+1
  for (i in 1:array_size){
    arr[GR_tmp[i], GC_tmp[i]] <- input[i]
  }

  if (spatialReference==T){
    
    corCol <- basinObject@cont@corCol
    corRow <- basinObject@cont@corRow
    #maybe here is a minor bug...that raster is shifted by one 
    xMin = (min(GC)+corCol-1)/12 - 180 #- 1/12
    xMax = (max(GC)+corCol-1)/12 - 180 + 1/12
    yMin = -((max(GR) + corRow - 1)/12 - 90) - 1/12
    yMax = -((min(GR) + corRow - 1)/12 - 90) #+ 1/24
    crsName <- raster::crs("+init=epsg:4326")
    arr <- raster::raster(arr, xmn=xMin, xmx=xMax, ymn=yMin, ymx=yMax, crs=crsName)
  } else {
    arr <- raster::raster(arr)
  }

  return(arr)
}

###############################################################################################################################

#' @title  create Average from vector or matrix of basin (vector needs ng entries)
#' @description #Functions to get average values of basin 
#' @param input input vector of basin e.g. SoilwaterContent from basinObject
#' @param basinObject basinObject itself that belongs to input 
#' @param Type as string (land or WB or all) to define which reference are should be used to calculated weighted mean as basin wide average (landfrac or 1-landfrac or 1) * GAREA
#' @return float or vector
#' @export
basin.createAverage <- function(input, basinObject, Type="land"){

  landfrac <- basinObject@landfrac
  GAREA <- basinObject@GAREA
  array_size <- length(GAREA)

  frac = switch(Type, "land" = landfrac, "WB"= 1-landfrac, "all" = 1) #NULL if Type is not defined

  if (is.vector(input) & length(input) == array_size){
      #calculation of Volume for landinput (given in mm) - for rest 1* is applied
      V = sum(input * GAREA * frac)
      #calculation of mm equivalent
      BasinMean = V / sum(GAREA)

  } else if (is.matrix(input) & dim(input)[2] == array_size) {
    BasinMean = sapply(1:dim(input)[1], function(x) sum(input[x,] * GAREA * frac) / sum(GAREA))
  } else {
    stop("input has a undefined type - should be vector or matrix with length of array_size and in case
         of a matrix length of timeseries as nrows")
  }
  return(BasinMean)
}

################################################################################################################################

#' @title basin.createWaterBalance
#' @description Function to create Waterbalance for basin
#' @param model.output output of the model (--> runModel())
#' @param climateObject climateObject which was used durign the simulation
#' @param basinObject basinObject itself that belongs to input 
#' @param start2use Date to use as start, given as string in the format "\%dd.\%mm.\%.YYYY"
#' @param end2use Date to use as end, given as string in the format "\%dd.\%mm.\%.YYYY"
#' @return List of waterbalance elements in the unit mm (referring to the total basin area)
#' @export
basin.createWaterBalance <- function(model.output, climateObject, basinObject, start2use=NULL, end2use=NULL){
  
  #check Date format
  if ( ( is.na(as.Date(start2use, format="%d.%m.%Y")) ) | ( is.na(as.Date(end2use, format="%d.%m.%Y")) ) ) {
    stop("Error using start2use or end2use - Please make sure that dates are given as string in the format %d.%m.%y or as NULL")
  }
  
  #getting indexof Date to examine
  if (is.null(start2use)) { start2use <- climateObject@start
  } else { 
    if (as.Date(start2use, format="%d.%m.%Y") < as.Date(climateObject@start, format="%d.%m.%Y")) stop("start2use is not defined properly, choose date wihtin simulaiton period")
  }
  
  if (is.null(end2use)) { end2use <-  climateObject@end
  } else { 
    if (as.Date(end2use, format="%d.%m.%Y") > as.Date(climateObject@end, format="%d.%m.%Y")) stop("end2use is not defined properly, choose date wihtin simulaiton period")
  }
  
  simTime <- seq(as.Date(climateObject@start, format="%d.%m.%Y"),as.Date(climateObject@end,format="%d.%m.%Y"), by=1) 
  usedTime <- seq(as.Date(start2use, format="%d.%m.%Y"),as.Date(start2use, format="%d.%m.%Y"), by=1) 
  n1 <- which(as.Date(start2use, format="%d.%m.%Y") == simTime)
  n2 <- which(as.Date(end2use, format="%d.%m.%Y") == simTime)
  
  
  #water use - Not sure if sign of water uses are correctly implemented!
  UseGW <- sum(model.output$daily$Fluxes$Flux_dailyWaterUseGW[n1:n2, ] * basinObject@landfrac * basinObject@GAREA)
  UseSW <- sum(model.output$routing$WaterUseSW[n1:n2, ] * basinObject@landfrac * basinObject@GAREA)
  if (UseGW + UseSW > 0) { stop("Please make sure that Uses are implemented with the correct sign - talk to Jenny Kupzig")}
  
  #landfraction
  prec_land <- sum(climateObject@prec[n1:n2, ] * basinObject@landfrac * basinObject@GAREA)
  ET_land <- sum((model.output$daily$Fluxes$InterceptionEvapo[n1:n2, ] + 
                    model.output$daily$Fluxes$Flux_Sublimation[n1:n2, ] +
                    model.output$daily$Fluxes$Flux_dailyAET[n1:n2, ]) * basinObject@landfrac * basinObject@GAREA)
  PET_land <- sum(model.output$daily$Fluxes$PET[n1:n2,] * basinObject@landfrac * basinObject@GAREA)
  if (PET_land < ET_land) { warning("Potential Evapotranspiration is smaller than actual Evapotranspiration, please check model equation!")}    
  S_start <- sum((model.output$daily$Storages$CanopyContent[n1,] +
                    model.output$daily$Storages$SnowContent[n1,] +
                    model.output$daily$Storages$SoilContent[n1,] +
                    model.output$daily$Storages$GroundwaterContent[n1,])* basinObject@landfrac * basinObject@GAREA)
  S_end   <- sum((model.output$daily$Storages$CanopyContent[n2,] +
                    model.output$daily$Storages$SnowContent[n2,] +
                    model.output$daily$Storages$SoilContent[n2,] +
                    model.output$daily$Storages$GroundwaterContent[n2,])* basinObject@landfrac * basinObject@GAREA)
  dS_land <- S_end -S_start    
  
  #waterfraction
  prec_water <- sum(climateObject@prec[n1:n2, ] * (basinObject@G_RESAREA + basinObject@G_LAKAREA) +
                      climateObject@prec[n1:n2, ] * (basinObject@G_LOCLAK + basinObject@G_LOCWET + basinObject@G_GLOWET)/100*basinObject@GAREA)
  ET_water <- sum(model.output$routing$locLake$Evapo[n1:n2, ] + 
                    model.output$routing$locWetland$Evapo[n1:n2, ] +
                    model.output$routing$gloLake$Evapo[n1:n2, ] +
                    model.output$routing$gloWetland$Evapo[n1:n2, ] +
                    model.output$routing$Res$Evapo[n1:n2, ]  )
  S_start <- sum( model.output$routing$locLake$Storage[n1, ] +
                    model.output$routing$locWetland$Storage[n1, ] +
                    model.output$routing$gloLake$Storage[n1, ] +
                    model.output$routing$gloWetland$Storage[n1, ] +
                    model.output$routing$Res$Storage[n1, ] + 
                    model.output$routing$River$RiverStorage[n1,])
  S_end    <- sum( model.output$routing$locLake$Storage[n2, ] +
                     model.output$routing$locWetland$Storage[n2, ] +
                     model.output$routing$gloLake$Storage[n2, ] +
                     model.output$routing$gloWetland$Storage[n2, ] +
                     model.output$routing$Res$Storage[n2, ] + 
                     model.output$routing$River$RiverStorage[n2,])   
  dS_water <- S_end -S_start    
  
  #outflow of basin
  Q <- sum(model.output$routing$River$Discharge[n1:n2])*sum(basinObject@GAREA)
  
  #Test difference of Precipitation amount due to inaccuracy and discrepancy in waterbodies (GLOLAK*GAREA ~ LAKAREA + RESAREA AND global/lokal waterbodies in percent not promille)
  prec_total <- sum(climateObject@prec[n1:n2, ] * basinObject@GAREA)
  if (prec_total != prec_land + prec_water) { warning(sprintf("Discrepancy in total precipitation of %f mm due to waterbodies", (prec_total - prec_land - prec_water) / sum(basinObject@GAREA)))}
  
  list2return <- list("Q"=Q, "P"=prec_land+prec_water, "E"=ET_water+ET_land, "dS"= dS_land + dS_water, "Uses"=UseGW+UseSW) # mm*km²
  list2return <- lapply(1:length(list2return), function (x) { list2return[[x]] / sum(basinObject@GAREA) } ) # mm
  names(list2return) <- c("Q", "P", "E", "dS", "Uses") #naming list
  
  if (list2return$P - list2return$Q - list2return$E - list2return$dS - list2return$Uses != 0) { 
    warning(sprintf("Waterbalance is not close, disrepancy of %f mm. \nNote that this could be due to storage issues because storage is always stored after simulation of day.", 
                    (list2return$P - list2return$Q - list2return$E - list2return$dS - list2return$Uses) / sum(basinObject@GAREA))) }
  
  return(list2return)
}