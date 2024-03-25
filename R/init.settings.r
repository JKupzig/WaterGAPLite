#' @title init.settings
#' @description helper function to create appropriate settings vector
#' @param water_use defines if water use is considered in model (on, off, including cities)
#' @param water_use_allocation defines how water use is considered if not satisfied (spatial and temporal, temporal, spatial)
#' @param flow_velocity defines if flow velocity is calculated variable (s. Verzano et al. 2009) or constant (constant, variable)
#' @param gap_year defined if 29.02 is considered are always ignored (included, not included)
#' @param reservoir_algorithm defines if reservoirs are considered using Hanasaki algorithm (this need estimated water use) (Hanasaki, as lakes)
#' @param use_splitting_factor new model option, splitting to groundwater can now be defined with parameter (off, on)
#' @param longwave_radiation defined if long wave radiation is calculated or read (reading, calculation)
#' @param initial_states defines how initial states are treated (not known, reading, writing, reading and writing)
#' @param snow_in_wetlands new model option, snow in wetland/freezing of wetlands can now be considered (on, off)
#' @param evaporation_from_river new model option, evaporation form river can now be considered (on, off)
#' @return settings vector to pass to run model
#' @export
init.settings <- function(water_use = "off",
                          water_use_allocation = "temporal and spatial",
                          flow_velocity = "constant",
                          gap_year = "included",
                          reservoir_algorithm = "as lakes",
                          use_splitting_factor = "off",
                          longwave_radiation = "reading",
                          initial_states = "not known",
                          snow_in_wetlands = "off",
                          evaporation_from_river = "off") {


  water_use_setting <- switch(
    water_use,
    "off" = 0, "on" = 1, "including cities" = 2
  )
  if (is.null(water_use_setting))
  {
    stop(sprintf("water use setting '%s' not valid choose between 'off', 'on', 'including cities'", water_use))
  }


  water_use_allocation_setting <- switch(
    water_use_allocation,
    "temporal and spatial" = 0, "spatial" = 1, "temporal" = 2
  )
  if (is.null(water_use_allocation_setting))
  {
    stop(sprintf("water use allocation setting '%s' not valid choose between 'temporal and spatial', 'spatial', 'temporal'", water_use_allocation))
  }


  flow_velocity_setting <- switch(
    flow_velocity,
    "constant" = 0, "variable" = 1
  )
  if (is.null(flow_velocity_setting))
  {
    stop(sprintf("flow velocity setting '%s' not valid choose between 'constant', 'variable'", flow_velocity))
  }


  gap_year_setting <- switch(
    gap_year,
    "included" = 0, "not included" = 1
  )
  if (is.null(gap_year_setting))
  {
    stop(sprintf("gap year setting '%s' not valid choose between 'included', 'not included'", gap_year))
  }


  reservoir_algorithm_setting <- switch(
    reservoir_algorithm,
    "Hanasaki" = 0, "as lakes" = 1)
  if (is.null(reservoir_algorithm_setting))
  {
    stop(sprintf("reservoir algorithm setting '%s' not valid choose between 'Hanasaki', 'as lakes'", reservoir_algorithm))
  }


  use_splitting_factor_setting <- switch(
    use_splitting_factor,
    "off" = 0, "on" = 1
  )
  if (is.null(use_splitting_factor_setting))
  {
    stop(sprintf("use splitting factor setting '%s' not valid choose between 'off', 'on'", use_splitting_factor))
  }


  longwave_radiation_setting <- switch(
    longwave_radiation,
    "reading" = 0, "calculation" = 1
  )
  if (is.null(longwave_radiation_setting))
  {
    stop(sprintf("long wave radiation setting '%s' not valid choose between 'reading', 'calculation'", longwave_radiation))
  }


  initial_states_setting <- switch(
    initial_states,
    "not known" = 0, "reading" = 1, "writing" = 2, "reading and writing" = 3
  )
  if (is.null(initial_states_setting))
  {
    stop(sprintf("initial states setting '%s' not valid choose between 'not known', reading', 'writing', 'reading and writing'", initial_states))
  }

  snow_in_wetland_setting <- switch(
    snow_in_wetlands,
    "off" = 0, "on" = 1
  )
  if (is.null(snow_in_wetland_setting))
  {
    stop(sprintf("snow in wetlands setting '%s' not valid choose between 'off', 'on'", snow_in_wetlands))
  }

  evaporation_from_river_setting <- switch(
    evaporation_from_river,
    "off" = 0, "on" = 1
  )
  if (is.null(evaporation_from_river_setting))
  {
    stop(sprintf("evaporation_from_river '%s' not valid choose between 'off', 'on'", evaporation_from_river))
  }

  settings_vector <- c(water_use_setting,
                       water_use_allocation_setting,
                       flow_velocity_setting,
                       gap_year_setting,
                       reservoir_algorithm_setting,
                       use_splitting_factor_setting,
                       longwave_radiation_setting,
                       initial_states_setting,
                       snow_in_wetland_setting,
                       evaporation_from_river_setting)

  return(settings_vector)

}
