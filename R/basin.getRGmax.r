#' @title Definition of Rg_max
#' @description Function to determin maximal recharge rate for groundwater
#' @param texture information from G_TEXTURE.UNF1
#' @return RG_max as maximal groundwater recharge rate in [mm/d]


basin.get_rgmax <- function(texture) {
  rg_max <- tools_interpolate(texture,
            c(10., 15., 20., 25., 30.),
            c(7., 5.75, 4.5, 3.5, 2.5)
            )

  #These are values for daily simulation (monthly have different values!)
  # 4.5 = others; 0 0 rock/glaciers
  rg_max[texture == 0] <- 4.5
  rg_max[texture == 2] <- 4.5
  rg_max[texture == 1] <- 0
  if (sum(rg_max == -999) > 0) {
    stop("Something is wrong with values in textureInfo -
          check if all are between 10 and 30 or have 0,1,2 as entries")
          }

  rg_max <- floor(rg_max * 100. + 0.5)
  return(rg_max)

}