#' @title  Initializing Basin as empty basinObject
#' @description function to initialize basin
#' @param continent_object ContinentObject defined from init.initCont()
#' @param grdc_number grdc number of basin
#' @param lat latitude of basin outlet in degree
#' @param long longitud of basin outlet in degree
#' @param cont continent as string (au, as, af, eu, na, sa)
#' @return created basinObject
#' @importFrom methods new
init.basin <- function(continent_object, grdc_number, lat, long, cont) {

  newbasin <- new("Basin",
                  id = grdc_number,
                  location = c(long, lat),
                  cont = continent_object)

  return(newbasin)
}