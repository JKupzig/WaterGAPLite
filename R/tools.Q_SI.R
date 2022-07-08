

################################################################################
# main functions
################################################################################
#' @title  Q.calcSI
#' @description applies SI-function to yearly ts and then return mean of all yearly values (to be more robust)
#' @param df dataframe with Sim and Date as columns
#' @param func_name string of function_name, default= "Q.__calc_mgn_l_1__"
#' @param addArgs additional argument e.g. when whole time series needs to be used it is passed as additonal Argument
#' @return float or integer as yearly mean of function return
#' @importFrom stats median
#' @importFrom stats quantile
#' @importFrom tibble enframe
#' @importFrom magrittr '%>%'
#' @details 
#' **func_names**:
#' \itemize{
#'   \item{"Q.__calc_mgn_l_1__":}{ \cr 5th quantile (or minimum, when 5th quantile is zero),  
#'         used in Pfannerstil 2014, McMillan 2021, Addor 2018 \cr 
#'         SI is referring to magnitude of low flow condition}
#'   \item{"Q.__calc_mgn_l_2__":}{ Seven-day minimum flow divided by mean annual daily flows averaged across all years
#'         used in Richter et al (1998), recommended in Olden and Poff (2003) \cr
#'         SI is referring to magnitude of low flow condition}
#'   
#'   \item{"Q.__calc_mgn_h_1__":}{ \cr 95th quantile of flow series, used in Addor (2018) \cr
#'         SI is referring to magnitude of high flow condition}
#'   \item{"Q.__calc_mgn_h_2__":}{ \cr Mean of the 10th percentile from the flow duration
#'         curve divided by median daily flow across all years, used in Clausen and Biggs (2000), 
#'         recommended after Olden and Poff (2003) \cr
#'         SI is referring to magnitude of high flow condition}
#'         
#'   \item{"Q.__calc_mgn_a_1__":}{ \cr calculates the skewness of daily values as mean(Q)/median(Q) 
#'         used in Clausen and Biggs 2000 and recommended after Olden and Poff (2003) \cr
#'         SI is referring to magnitude of average flow condition}
#'   \item{"Q.__calc_mgn_a_2__":}{ \cr average of flow (m³/s) and returns it in mm/d 
#'         used in Hughes and James (1989), Addor 2018 and recommended in Olden and Poff (2003) \cr
#'         SI is referring to magnitude of average flow condition}
#'   
#'   \item{ "Q.__calc_frq_l_1__":}{ \cr Frequency of low-flow days in a year (<0.2 times the mean daily flow) 
#'         used in Addor 2018 \cr
#'         SI is referring to frequency of low flow condition}
#'   \item{"Q.__calc_frq_l_2__":}{ \cr Total number of low flow spells, threshold to define low flow is equal to 5% of
#'         mean daily flow divided by the record length in years, used in Huges and James 1989, 
#'         recommended in Olden and Poff (2003) \cr
#'         SI is referring to frequency of low flow condition \cr 
#'         **Note: might not be appropriate for other basins than Australian like basins**}
#'   
#'   \item{"Q.__calc_frq_h_1__":}{ \cr Frequency of high-flow days (>9 times the median daily flow)
#'         used in Addor (2018)\cr
#'         SI is referring to frequency of high flow condition}
#'   \item{"Q.__calc_frq_h_2__":}{ \cr Number of high flow events per year using an upper
#'         threshold of 3 times median flow over all years,
#'         used in Clausen and Biggs 2000, recommended in Olden and Poff (2003) \cr
#'         SI is referring to frequency of high flow condition}
#'  
#'   \item{"Q.__calc_dur_l_1__":}{ \cr Average duration of low-flow events, 
#'          ,i.e. number of consecutive days <0.2
#'         times the  mean daily flow used in Addor 2018 \cr
#'         SI is referring to duration of low flow condition}
#'   \item{""Q.__calc_dur_l_2__":}{ \cr Mean annual 30-day minimum, divided by median flow, 
#'        used in Clausen 2000, recommended in Olden and Poff (2003) \cr
#'        SI is referring to duration of low flow condition}
#'   
#'   \item{"Q.__calc_dur_h_1__":}{ \cr number of consecutive days >9 times the median daily flow), used in Addor 2018 \cr
#'         SI is referring to duration of high flow condition }
#'   \item{"Q.__calc_dur_h_2__":}{ \cr 30-day maxima of daily discharge divided by median flow, 
#'         used in Clausen  2000, recommended in Olden and Poff (2003) \cr
#'          SI is referring to duration of high flow condition}
#'   
#'   \item{"Q.__calc_timing_1__":}{ \cr date on which the cumulative discharge since January first reaches
#'         half of the annual discharge, used in Court 1962, Addor 2018 \cr
#'          SI is referring to duration of timing \cr
#'         **Note: slightly modified, before October was used not January**}
#'   \item{"Q.__calc_timing_2__":} { \cr Variability in Julian date of annual Minimum expressed as coefficient of variation \cr
#'         recommended in Olden and Poff (2003), original SI used in Richter et al (1998) \cr
#'         SI is referring to duration of timing}
#'   \item{"Q.__calc_timing_3__":} { \cr mean Julian date of annual Minimum, used in Richter et al (1998) - \cr
#'         not recommended just used as proof \cr
#'         SI is referring to duration of timing}
#'   
#'   \item{"Q.__calc_rchg_1__":}{ \cr Slope of the flow duration curve (between
#'    the log-transformed 33rd and 66th stream flow percentiles, \cr
#'    used in Addor 2018, Sawizc 2011 \cr
#'    SI is referring to duration of rate of change}
#'   \item{"Q.__calc_rchg_2__":}{ \cr Ratio of days where the flow is higher than the previous day, \cr
#'         recommended in Olden and Poff (2003) \cr
#'         SI is referring to duration of rate of change}
#'   
#'}   
#' @examples 
#' Date = seq(as.Date("01.01.1980", format="%d.%m.%Y"), as.Date("31.12.1989", format="%d.%m.%Y"),1)
#' df = data.frame("Sim"=rnorm(length(Date), mean=50, sd=20), "Date"=Date)
#' 
#' mgn_l_1 = Q.calcSI(df, func_name="Q.__calc_mgn_l_1__") 
#' mgn_l_2 = Q.calcSI(df, func_name="Q.__calc_mgn_l_2__")

#' mgn_a_1 = Q.calcSI(df, func_name="Q.__calc_mgn_a_1__") 
#' mgn_a_2 = Q.calcSI(df, func_name="Q.__calc_mgn_a_2__", addArgs = 80) 

#' mgn_h_1 = Q.calcSI(df, func_name="Q.__calc_mgn_h_1__") 
#' mgn_h_2 = Q.calcSI(df, func_name="Q.__calc_mgn_h_2__", addArgs = df$Sim) 

#' frq_l_1 = Q.calcSI(df, func_name="Q.__calc_frq_l_1__", addArgs = df$Sim) 
#' frq_l_2 = Q.calcSI(df, func_name="Q.__calc_frq_l_2__", addArgs = df$Sim) 

#' frq_h_1 = Q.calcSI(df, func_name="Q.__calc_frq_h_1__", addArgs = df$Sim)  
#' frq_h_2 = Q.calcSI(df, func_name="Q.__calc_frq_h_2__", addArgs = df$Sim)

#' dur_l_1 = Q.calcSI(df, func_name="Q.__calc_dur_l_1__", addArgs = df$Sim) 
#' dur_l_2 = Q.calcSI(df, func_name="Q.__calc_dur_l_2__", addArgs = df$Sim) 

#' dur_h_1 = Q.calcSI(df, func_name="Q.__calc_dur_h_1__", addArgs = df$Sim)  
#' dur_h_2 = Q.calcSI(df, func_name="Q.__calc_dur_h_2__", addArgs = df$Sim)
#' 
#' timing_1 = Q.calcSI(df, func_name="Q.__calc_timing_1__")
#' timing_2 = Q.calcSI(df, func_name="Q.__calc_timing_2__", addArgs=df) 
#' timing_3 = Q.calcSI(df, func_name="Q.__calc_timing_3__")

#' rchg_1 = Q.calcSI(df, func_name="Q.__calc_rchg_1__") 
#' rchg_2 = Q.calcSI(df, func_name="Q.__calc_rchg_2__") 
#' @export
#' @md 
Q.calcSI <- function(df, func_name="Q.__calc_mgn_l_1__", addArgs=NULL){
  
  '%>%' <- magrittr::'%>%'
  
  value = df %>% dplyr::mutate(year = format(.data$Date, "%Y")) %>%
    dplyr::group_by(.data$year) %>%
    dplyr::group_modify(~ {
      get(func_name)(.x$Sim, addArgs) %>%
        tibble::enframe()
    }) %>%
    dplyr::ungroup() %>%
    dplyr::summarize(mean = mean(value)) %>% 
    as.numeric()
  return(value)
}

################################################################################
# helper functions
################################################################################
#' @title get_periods
#' @description defines consecutive entries with 1 as entries (0s are gaps between periods)
#' @param x vector with binary entries, where ones are than summed up to periods
#' @return data.frame with start end end (index)
get_periods <- function(x) {
  x <- base::rle(as.vector(x))
  lens <- x$lengths
  ends <- cumsum(lens)[x$values]
  starts <- ends - lens[x$values] + 1L
  data.frame(
    period = seq_along(starts), 
    period_start = starts, 
    period_end = ends
  )
}

#' @title moving average (ma)
#' @description function to calculate moving average, using 7 entries as defualt moving 
#' @param x number of entries to be used for mean calculation (should be uneven, because average is defined as value in the middle)
#' @param n number of entreis within the moving window, the default is 7
#' @return vector, starting and ending with NA due to too less entries at the start and end for ma-calculation
#' @export
#' 
ma <- function(x, n = 7){
  return(stats::filter(x, rep(1 / n, n), sides = 2))
}

################################################################################
# magnitude low flow condition
################################################################################
#' @title 5th quantile
#' @description returns 5th quantile (or minimum, when 5th quantile is zero),   
#' SI used in Pfannerstil 2014, McMillan 2021, Addor 2018; SI referring to magnitude of low flow condition
#' @param discharge input vector, e.g. usually disharge values in m?/s
#' @param addArgs NULL (not used, but implemented to be useable for Q.calcSI)
#' @return float 
Q.__calc_mgn_l_1__ <- function(discharge, addArgs=NULL){
  return(max(stats::quantile(discharge, 0.05),min(discharge[discharge > 0])))
}

#' @title Calculate Baseflow Index after Richter et al. (1998)
#' @description Seven-day minimum flow divided by mean annual daily flows averaged across all years 
#' used in Richter et al. (1998), recommended in Olden & Poff (2003)   
#' SI referring to magnitude of low flow condition
#' @param discharge A numeric vector to be used to calculate the BFI
#' @param addArgs NULL (not used, but implemented to be useable for Q.calcSI)
#' @return A numeric value representing the BFI of the numeric vector provided
Q.__calc_mgn_l_2__ <- function(discharge, addArgs=NULL){ #O + P 2003
  rollmean <- ma(discharge)
  return(min(rollmean, na.rm=T)/mean(discharge))
}

################################################################################
# magnitude average condition
################################################################################
#' @title skewness of daily values
#' @description calculates the skewness of daily values as mean(Q)/median(Q) 
#' used in Clausen and Biggs 2000 and recommended after Olden and Poff (2003)
#' @param discharge A numeric vector to be used to calculate the BFI
#' @param addArgs NULL (not used, but implemented to be useable for Q.calcSI)
#' @return A numeric value representing the BFI of the numeric vector provided
Q.__calc_mgn_a_1__ <- function(discharge, addArgs=NULL){
  #Clausen and Biggs 2000
  return(mean(discharge)/stats::median(discharge))
}

#' @title mean(q) in mm/d
#' @description calculates average of flow (m³/s) and returns it in mm/d 
#' used in Hughes and James (1989), Addor 2018 and recommended in Olden and Poff (2003)
#' @param discharge vector in m³/s
#' @param area of basin defined in km² as float or integer
#' @return return mean flow in mm/d as float
Q.__calc_mgn_a_2__ <- function(discharge, area){ #O + P 2003
  #mean(q)/area 	Hughes and James (1989), Addor 2018 --> mm/day
  return(mean(discharge)*60*60*24/area/1000)
}

################################################################################
# magnitude high flow condition
################################################################################

#' @title 95th quantile
#' @description calculates 95th quantile of flow series, used in Addor (2018) \cr 
#' is a SI referrring to magnitude of high flow condition
#' @param discharge vector in m³/s
#' @param addArgs NULL (not used, but implemented to be useable for Q.calcSI)
#' @return 95th quantile 
Q.__calc_mgn_h_1__ <- function(discharge, addArgs=NULL){
  #Q5 	Addor 2018
  return(stats::quantile(discharge, 0.95))
}

#' @title Q90_year/Q50_total
#' @description Mean of the 90th percentile from the flow duration \cr
#  curve divided by median daily flow across all years, used in Clausen and Biggs (2000), 
#' recommended after Olden and Poff (2003)
#' @param discharge vector in m³/s (usually for one year)
#' @param completeDischarge vector in m³/s (usually for whole period)
#' @return float Q10_year/Q50_total
Q.__calc_mgn_h_2__ <- function(discharge, completeDischarge){ #O + P 2003
  # Q10/Q50 	Clausen and Biggs 2000
  return(stats::quantile(discharge, 0.9) / stats::quantile(completeDischarge, 0.5))
}

################################################################################
# frequency low flow condition
################################################################################

#' @title Frequency of low-flow days (<0.2 times the mean daily flow)
#' @description Frequency of low-flow days in a year (<0.2 times the mean daily flow) used in Addor 2018
#' @param discharge vector in m³/s (usually for one year)
#' @param completeDischarge vector in m³/s (usually for whole year) - necessary to define threshold
#' @return days per year above threshold
Q.__calc_frq_l_1__ <- function(discharge, completeDischarge){
  threshold = mean(completeDischarge)*0.2
  idx = (discharge < threshold)
  return(sum(idx))
}

#' @title Number of low flow spells
#' @description mean number of low flow spells (threshold equal to 5% of
#  mean daily flow) divided by the record length in years -> #/yr
#  note: might not be appropiate for other basins than australien like basins as used in Huges and James 1989
#' @param discharge vector in m³/s - actually not used, so could be set to NULL
#' @param completeDischarge vector in m³/s (usually for whole year) - necessary to define threshold
#' @return number of low flow periods in examined year
Q.__calc_frq_l_2__ <- function(discharge, completeDischarge){ #O + P 2003
  threshold = mean(completeDischarge)*0.05
  idx = (discharge < threshold)
  df_periods = get_periods(idx)
  return(nrow(df_periods))
}

################################################################################
# frequency high flow condition
################################################################################

#' @title Frequency of high-flow days 
#' @description Frequency of high-flow days (>9 times the median daily flow)
#' used in Addor (2018)
#' @param discharge vector in m³/s (usually for one year)
#' @param completeDischarge vector in m³/s (usually for whole period) - necessary to define threshold 
#' @return returns number of days where threshold is exceeded in whole period
Q.__calc_frq_h_1__ <- function(discharge, completeDischarge){
  threshold = stats::median(completeDischarge)*9
  idx = (discharge > threshold)
  return(sum(idx))
}

#' @title Number of high flow events
#' @description Number of high flow events per year using an upper
#' threshold of 3 times median flow over all years,
#' used in Clausen and Biggs 2000, recommended in Olden and Poff (2003)
#' @param discharge vector in m³/s (usually for one year)
#' @param completeDischarge vector in m³/s (usually for whole year) - necessary to define threshold 
#' @return days in examined year
Q.__calc_frq_h_2__ <- function(discharge, completeDischarge){ #O + P 2003
  threshold = stats::median(completeDischarge)*3
  idx = (discharge > threshold)
  df_periods = get_periods(idx)
  return(nrow(df_periods))
}


################################################################################
# duration low flow condition 
################################################################################

#' @title Average duration of low-flow events
#' @description Average duration of low-flow events
#  (number of consecutive days <0.2 times the
#  mean daily flow), used in Addor 2018
#' @param discharge vector in m³/s (usually for one year)
#' @param completeDischarge vector in m³/s (usually for whole period) - necessary to define threshold 
#' @return mean number of consecutive days with flow lower than defined threshold
Q.__calc_dur_l_1__ <- function(discharge, completeDischarge){
  threshold = mean(completeDischarge)*0.2
  idx = (completeDischarge < threshold)
  df_periods = get_periods(idx)
  df_periods$delta = df_periods$period_end - df_periods$period_start
  if (length(df_periods$delta) == 0){
    val = 0
  } else {
    val = mean(df_periods$delta) 
  }
  return(val)
}

#' @title adjusted Mean annual 30-day minimum
#' @description Mean annual 30-day minimum, divided by median flow, 
#' used in Clausen 2000, recommended in Olden and Poff (2003)
#' @param discharge vector in m³/s (usually for one year)
#' @param completeDischarge vector in m³/s (usually for whole period) - necessary to adjust data 
#' @return float 
Q.__calc_dur_l_2__ <- function(discharge, completeDischarge){ #O + P 2003
  NM30Q = min(ma(discharge, 30), na.rm=T)
  return(NM30Q/stats::median(completeDischarge))
}

################################################################################
# duration high flow condition
################################################################################

#' @title Average duration of high-flow events
#' @description number of consecutive days >9 times the median daily flow), used in Addor 2018
#' @param discharge vector in m³/s (usually for one year)
#' @param completeDischarge vector in m³/s (usually for whole period) - necessary to define threshold 
#' @return mean duration in days (float) for the examined year
Q.__calc_dur_h_1__ <- function(discharge, completeDischarge){
  threshold = stats::median(completeDischarge)*9
  idx = (discharge > threshold)
  df_periods = get_periods(idx)
  df_periods$delta = df_periods$period_end - df_periods$period_start
  if (length(df_periods$delta) == 0){
    val = 0
  } else {
    val = mean(df_periods$delta) 
  }
  return(val)
}

#' @title adjusted 30-day maxima of daily discharge
#' @description 30-day maxima of daily discharge divided by median flow, 
#' used in Clausen  2000, recommended in Olden and Poff (2003)
#' @param discharge vector in m³/s (usually for one year)
#' @param completeDischarge vector in m³/s (usually for whole period) - necessary to adjust value
#' @return adjusted 30-day maxima (float)
Q.__calc_dur_h_2__ <- function(discharge, completeDischarge){ #O + P 2003
  MM30Q = max(ma(discharge, 30), na.rm=T)
  return(MM30Q/stats::median(completeDischarge))
}

################################################################################
# timing
################################################################################

#' @title Mean half-flow date
#' @description date on which the cumulative discharge since January first reaches
#'  half of the annual discharge, used in Court 1962, Addor 2018
#'  Note: slightly modified, befroe Ocotber was used not January
#' @param discharge vector in m³/s (usually for one year)
#' @param addArgs NULL (not used, but implemented to be useable for Q.calcSI)
#' @return DOY from date when for the first time more then halb ofthe water volume passed
Q.__calc_timing_1__ <- function(discharge, addArgs=NULL){
  threshold = sum(discharge)*0.5
  return(min(which(cumsum(discharge) >= threshold)))
}

#' @title Variability in Julian date of annual Minimum
#' @description Variability in Julian date of annual Minimum expressed as coefficient of variation
#' @param discharge vector in m³/s (usually for one year) - actually not used wihtin this function
#' @param df dataframe of simulated discharge with (at least) 'Date' and 'Sim' (or 'Value') column
#' @return coefficient of variation of DOY of the yearly annual minima
Q.__calc_timing_2__ <- function(discharge, df){ #O + P 2003
  # Variability in Julian date of annual Minimum expressed as coefficient of variation
  df$year <- format(df$Date, "%Y")
  minYear <- min(df$year); maxYear <- max(df$year)
  DOY <- lapply(minYear:maxYear, function(x) { which(df$Sim[df$year==x] == min(df$Sim[df$year==x]))} )
  CV = sd(unlist(DOY))/(mean(unlist(DOY)))
  return(CV)
}

#' @title mean Julian date of annual Minimum 
#' @description mean Julian date of annual Minimum, used in Richter et al (1998) - not recommended just used as proof
#' @param discharge vector in m³/s (usually for one year)
#' @param addArgs NULL (not used, but implemented to be useable for Q.calcSI)
#' @return DOY of annual minimum of examined year
Q.__calc_timing_3__ <- function(discharge, addArgs=NULL){
  # 
  DOY <- which(discharge == min(discharge))
  return(DOY)
}


################################################################################
# rate of change
################################################################################

#' @title slope of FDC
#' @description Slope of the flow duration curve (between
#' the log-transformed 33rd and 66th streamflow percentiles Addor 2018, sawizc 2011
#' @param discharge vector in m³/s (usually for one year)
#' @param addArgs NULL (not used, but implemented to be useable for Q.calcSI)
#' @return slope value [-]
Q.__calc_rchg_1__ <- function(discharge, addArgs=NULL){
  # Slope of the flow duration curve (between
  # the log-transformed 33rd and 66th streamflow percentiles Addor 2018, sawizc 2011
  Q33 <- as.numeric(stats::quantile(discharge, 0.33, na.rm=T))
  Q66 <- as.numeric(stats::quantile(discharge, 0.66, na.rm=T))
  val <- (log(Q66) - log(Q33)) / (0.66 - 0.33)
  return(val)                                  
}

#' @title Ratio of days where the flow is higher than the previous day
#' @description  Ratio of days where the flow is higher than the previous day, recommended in Olden and Poff (2003)
#' @param discharge vector in m³/s (usually for one year)
#' @param addArgs NULL (not used, but implemented to be useable for Q.calcSI)
#' @return number of days 
Q.__calc_rchg_2__ <- function(discharge, addArgs=NULL){ #O + P 2003
  # Ratio of days where the flow is higher than the previous day
  return(sum(discharge[1:(length(discharge)-1)] > discharge[2:(length(discharge))]))                                  
}



