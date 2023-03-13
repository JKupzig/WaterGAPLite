#' @title Initializing model to run for specified basin
#' @description Function to initialize the model (all time-independent data is load for basin) 
#' @param grdc_number id for basin, usually grdc number is used because this id is also used for loading discharge data
#' @param lat latitude of basin outlet in degree
#' @param long longitud of basin outlet in degree
#' @param cont continent as string where basin is part of (as, af, au, na, eu, sa)
#' @param base filepath to folder where folder for data, source_code and output are located
#' @return new basin object for basin (continent object is part of basin object)
#' @export

init.model <- function(grdc_number, lat, long, cont, base){
  
  #init.loadPackages() #loading packages --> is now part of DESCRIPTION
  init.classes() #directly initializing classes
  
  Continent <- init.initCont(cont=cont,path2base=base)
  Basin <- init.basin(Continent, grdc_number = grdc_number, 
                       lat=lat, long=long)
  
  
   Basin <- basin.defBasin(Basin) #creates arraysize, basinIndex, grcrWithout..
   Basin <- basin.getBasinInfo(Basin)
   Basin <- basin.getRoutingInfo(Basin)
   Basin <- basin.getNeighbourcells(Basin)
  
   #Parameter setting
   DataDir <- slot(Basin, "cont")@DataDir
   folder <- file.path(DataDir, "basinInfo", cont)
   LCT <- data.table::fread(file.path(folder, "LCT_31.DAT"), skip=28, 
                            col.names=c("LCT", "rootindDepth", "albedo", "snowAlbedo", "degreeDayFactor", "emissivity"))
   LAI <- data.table::fread(file.path(folder, "LAI_31.DAT"), skip=29,
                            col.names=c("LCT", "leafArea", "frDecPlant", "red_frDecPlant", "initDays", "kc_min", "kc_max"))
   Basin <- init.SetParsBasin(Basin, LAI, LCT) 
   
  
  return(Basin)
  
}
