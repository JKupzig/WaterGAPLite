#' @title Definition of model parameter values for basinObject
#' @description Function to set parameters of model to basin
#' @param basin_object that is updated with parameters
#' @param lai_info table with LAI_info (is then used to define parameters)
#' @param lct_info table with LCT info (is then used to define parameters)
#' @return basin object with defined parameters
#' for basin (continent object is part of basin object)
#' @importFrom methods slot<-
init.set_pars_basin <- function(basin_object, lai_info, lct_info) {

  par_table <- init.setPars()
  
  for (i in 6:nrow(par_table))
  {
    slot(basin_object, par_table[i, 1]) <- as.numeric(par_table[i, 2])
  }

  #LAI table
  landcover <- basin_object@GLCT
  basin_object@LAI_max <- unlist(lai_info[landcover, 2], use.names = FALSE)
  basin_object@initDays <- unlist(lai_info[landcover, 5], use.names = FALSE)
  reduction_factor_dec_plant <- unlist(lai_info[landcover, 4],
                                        use.names = FALSE)
  factor_dec_plant <- unlist(lai_info[landcover, 3], use.names = FALSE)

  ##LAI_min calculation (original from model code in lai.cpp ll. 93-107)
  lai_factor_a <- 0.1 * factor_dec_plant
  lai_factor_b <- (1 - factor_dec_plant) * reduction_factor_dec_plant
  basin_object@LAI_min <- lai_factor_a + lai_factor_b * basin_object@LAI_max

  #LCT table & arid/humid info
  basin_object@alphaPT <- ifelse(basin_object@G_ARID_HUMID == 1,
                                as.numeric(par_table[1, 2]),
                                as.numeric(par_table[2, 2])
                                )
  basin_object@maxDailyPET <- ifelse(basin_object@G_ARID_HUMID == 1,
                                as.numeric(par_table[3, 2]),
                                as.numeric(par_table[4, 2])
                                )

  basin_object@emissivity <- unlist(lct_info[landcover, 6], use.names = FALSE)
  basin_object@albedo <- unlist(lct_info[landcover, 3], use.names = FALSE)
  basin_object@albedoSnow <- unlist(lct_info[landcover, 4], use.names = FALSE)
  basin_object@rootingDepth <- unlist(lct_info[landcover, 2], use.names = FALSE)
  basin_object@degreeDayFactor <- unlist(lct_info[landcover, 5],
                                          use.names = FALSE)

  #SplitFactpr
  basin_object@Splitfactor <- rep(as.numeric(par_table[5, 2]), 
                                  length(landcover))

  #Soil informatiom
  if (sum(basin_object@G_BATJES < 0) > 0) {
    warning("There are negative Values in G_BATJES -
            these are replaced by 10")
  }

  basin_object@G_BATJES <- ifelse(basin_object@G_BATJES < 0,
                                   10,
                                   basin_object@G_BATJES
                                  )

  basin_object@G_Smax <- basin_object@G_BATJES * basin_object@rootingDepth
                         

  #info to simulate/estimate groundwater recharge
  basin_object@G_RG_max <- basin.get_rgmax(basin_object@G_TEXTURE)
  basin_object@G_gwFactor <- basin.get_gw_factor(basin_object@G_TEXTURE,
                                                 basin_object@G_SLOPE_CLASS,
                                                 basin_object@G_PERMAGLAC,
                                                 basin_object@G_AQ_FACTOR)

  basin_object@landfrac <- 1 - (basin_object@G_GLOLAK +
                                basin_object@G_GLOWET +
                                basin_object@G_LOCLAK +
                                basin_object@G_LOCWET) / 100

  return(basin_object)
}
