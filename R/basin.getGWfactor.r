#' @title Defining Groundwater Recharge Factor
#' @description Function to prepare and get groundwater related parameter
#' (gwFactor = aquiferFactor x permaFactor x slopeFactor x textureFactor)
#' @param texture - information from G_TEXTURE.UNF1
#' @param slope - information from G_SLOPE_CLASS.UNF1
#' @param permafrost - information from G_PERMAGLAC.UNF1
#' @param aquifer - information from G_AQ_FACTOR.UNF1
#' @return groundwater factor to determine groundwater recharge


basin.get_gw_factor <- function(texture, slope, permafrost, aquifer) {

  aquifer_factor <- aquifer / 100. #has to be limited to 0-1
  if ((sum(aquifer_factor > 1) + sum(aquifer_factor < 0)) > 0) {
    stop("Something is wrong with values in aquiferInfo -
          check if all are between 0 and 100")
  }

  perma_factor   <- 1 - (permafrost / 100.)  #has to be limited to 0-1
  if ((sum(perma_factor > 1) + sum(perma_factor < 0)) > 0) {
    warning("Something is wrong with values in permaInfo -
            check if all are between 0 and 100")
  }
  perma_factor[perma_factor < 0] <- 0 #correct values
  perma_factor[perma_factor > 1] <- 1 #correct values

  slope_factor <- tools_interpolate(slope,
                  c(10, 20, 30, 40, 50, 60, 70),
                  c(1., 0.95, 0.9, 0.75, 0.6, 0.3, 0.15)
                  )

  if (sum(slope_factor == -999) > 0) {
    stop("Something is wrong with values in SlopeClass -
          check if all are between 10 and 70")
  }

  texture_factor <- tools_interpolate(texture,
                    c(10, 15, 20, 25, 30),
                    c(1, 0.975, 0.95, 0.825, 0.7)
                    )

  # 0.95 fpr others and 0 for rock/glaciers
  texture_factor[texture == 0] <- 0.95
  texture_factor[texture == 2] <- 0.95
  texture_factor[texture == 1] <- 0
  if (sum(texture_factor == -999) > 0) {
    stop("Something is wrong with values in textureInfo -
          check if all are between 10 and 30 or have 0,1,2 as entries")
  }

  gw_factor <- aquifer_factor * perma_factor * slope_factor * texture_factor
  return(gw_factor)
}
