#' @title Initializing model to run for specified basin
#' @description Function to initialize the model
#' all time-independent data is load for basin)
#' @param grdc_number id for basin, usually grdc
#' number is used because this id is also used for loading discharge data
#' @param lat latitude of basin outlet in degree
#' @param long longitud of basin outlet in degree
#' @param cont continent as string where basin is
#' part of (as, af, au, na, eu, sa)
#' @param base filepath to folder where folder
#' for data, source_code and output are located
#' @return new basin object for basin (continent object is part of basin object)
#' @export
init.model <- function(grdc_number, lat, long, cont, base) {

  col_names_lct <- c("LCT", "rootindDepth", "albedo",
                  "snowAlbedo", "degreeDayFactor", "emissivity")
  col_names_lai <- c("LCT", "leafArea", "frDecPlant", "red_frDecPlant",
              "initDays", "kc_min", "kc_max")

  init.classes() #directly initializing classes

  continent <- init.init_cont(cont = cont, path2base = base)
  basin <- init.basin(continent, grdc_number = grdc_number,
                       lat = lat, long = long)

   basin <- basin.def_basin(basin) #creates arraysize, basinIndex, grcrWithout..
   basin <- basin.get_basin_info(basin)
   basin <- basin.get_routing_info(basin)
   basin <- basin.get_neighbourcells(basin)

   #Parameter setting
   DataDir <- slot(basin, "cont")@DataDir
   folder <- file.path(DataDir, "basinInfo", cont)
   LCT <- data.table::fread(file.path(folder, "LCT_31.DAT"), skip = 28,
                            col.names = col_names_lct)
   LAI <- data.table::fread(file.path(folder, "LAI_31.DAT"), skip = 29,
                            col.names = col_names_lai)
   basin <- init.set_pars_basin(basin, LAI, LCT)

  return(basin)

}
