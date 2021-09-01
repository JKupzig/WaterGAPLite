#' @title tools.prepareFolderStructur
#' @description prepare FolderStructure to prepare model run with package
#' @param base filepath that is used as base to prepare folder structure
#' @export
tools.prepareFolderStructur <- function(base){
  
  conts <- c("au", "af", "as", "eu", "na", "sa")
  
  dir.create(file.path(base, "data"))
  dir.create(file.path(base, "output"))
  dir.create(file.path(base, "data", "basinInfo"))
  dir.create(file.path(base, "data", "calibration"))
  dir.create(file.path(base, "data", "changingFiles"))
  dir.create(file.path(base, "data", "climate"))
  dir.create(file.path(base, "data", "climate", "continental"))
  dir.create(file.path(base, "data", "climate", "global"))
  dir.create(file.path(base, "data", "climate", "basin"))
  dir.create(file.path(base, "data", "climateShortCut"))
  dir.create(file.path(base, "data", "routing"))
  dir.create(file.path(base, "data", "waterUse"))
  
  for (cont in conts ) {
    dir.create(file.path(base, "output", cont))
    
    dir.create(file.path(base, "data", "basinInfo", cont))
    dir.create(file.path(base, "data", "calibration", cont))
    dir.create(file.path(base, "data", "changingFiles", cont))
    dir.create(file.path(base, "data", "climate", "continental", cont))
    dir.create(file.path(base, "data", "climateShortCut", cont))
    dir.create(file.path(base, "data", "routing", cont))
    dir.create(file.path(base, "data", "waterUse", cont))
  }
  
  #now writing continentInfo.csv
  data <- WaterGAPLite:::ContinentInfo
  utils::write.table(data, file.path(base, "data", "ContinentInfo.csv"), sep=";", row.names = F, col.names=T)
  
}

#tools.prepareFolderStructur(base)
