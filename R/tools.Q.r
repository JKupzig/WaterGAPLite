#' @title Reading GRDC data
#' @description Functions to read discharge data
#' @param basin_object_id grdc_number of basin, can be obtained from basinObject
#' @param data_dir dataDirection as string can be obtained from basinObject
#' @param cont continent as string (au, af, as, na, eu, sa)
#' @param start start data as date to read GRDC data
#' @param end end date as date to read GRDC data
#' @param use_folder if defined another folder than data/calibration/cont can be
#' set to look for discharge data (GRDC-like data)
#' @return dataframe with Date and Q as given in GRDC-file with name
#' "(grdc_number)_Q_Day.Cmd.txt"
#' @importFrom utils read.csv2
#' @import ggplot2
#' @export

Q.read_grdc <- function(basin_object_id, data_dir, cont,
                        start, end, use_folder = NULL) {

  start <- as.Date(start, "%d.%m.%Y")
  end <- as.Date(end, "%d.%m.%Y")
  folder <- file.path(data_dir, "calibration", cont)

  if (!is.null(use_folder)) {
    folder <- use_folder
  }

  path2read <- file.path(folder,
              paste0(as.character(basin_object_id), "_Q_Day.Cmd.txt"))

  data <- read.csv2(path2read, skip = 37, sep = ";", header = FALSE,
                    dec = ".", colClasses = c("Date", NA, "numeric"))

  colnames(data) <- c("Date", "Time", "Value")

  timeseq <- data.frame("Date"=seq(start, end, by = 1))
  data_complete <- merge(timeseq, data, by = "Date", all.x = TRUE)
  data_complete$Value[data_complete$Value < 0] <- NA


  start_new <- max(min(data[["Date"]]), start)
  end_new   <- min(max(data[["Date"]]), end)
  countNA <- length(which(is.na(data_complete[["Value"]])))

  #informing user if there are NAs in timeseries
  if (countNA > 0) {
    warning("NoData Values (< 0) are filled with NA")
  }
  if (start_new != start) {
    warning(paste("Read timeseries is to short, it starts at",
                  start_new, "NA introduced"))
  }
  if (end_new != end) {
    warning(paste("Read timeseries is to short, it ends at",
    end_new,"NA introduced"))
  }

  return(data_complete)
}

################################################################################
#' @title m³/s to mm/d
#' @description Functions to convert m³/s to mm/d
#' @param timeseries vector/matrix of data that needs to be converted
#' @param area sum of GAREA information of basin, (area must be in km²)
#' @return changed timeseries
#' @export
#'
Q.convert_m3s_mmday <- function(timeseries, area) {

  timeseries <- timeseries * 60 * 60 * 24 / area / 1000

  return(timeseries)

}

################################################################################
#' @title mm/d to m³/s
#' @description Functions to convertmm/d to m³/s
#' @param timeseries vector/matrix of data that needs to be converted
#' @param area GAREA information of basin (area must be in km²!)
#' @return changed timeseries
#' @export
Q.convert_mmday_m3s <- function(timeseries, area) {

  timeseries <- timeseries / (60 * 60 * 24) * area * 1000
  return(timeseries)
}

################################################################################
#' @title Calculating Benchmark
#' @description Function to calculate several benchmarks to compare simulated
#' and observed discharge. NA or negative values in observed data are omitted.
#' @param df_obs data.frame of observed discharge; "Date" and "Value" columns
#' @param df_sim data.frame of simulated discharge; "Date" and "Sim" Columns
#' @param type string (QmeanAbs, NSE, logNSE, KGE or pBias)
#' \itemize{
#'   \item QmeanAbs: absolute differenc between mean values of sim and obs
#'   \item NSE: Nash-Sutcliff-Efficiency
#'   \item logNSE: NSE of logarithmic values
#'   \item KGE: Kling-Gupta-efficiency and its components (Gupta et al. (2009))
#'   \item KGE_2012: mod. Kling-Gupta-efficiency and its components, recommended by Cinkus et al. (2023) (CV is used for variablitity; Gupta et al. (2012))
#'   \item pBias:  relative bias as described in van Werkhoven et al. 2008
#'   \item mod_index_of_agreement: Modified index of agreement (Willmott et al., 1985), recommended by Cinkus et al. (2023)
#' }
#' @param min_data minimum on available observed data in examined timeperiod, \cr
#' e.g. 0.5 for 50 percent (default)
#' @return named list with benchmarks
#' @importFrom stats cor
#' @importFrom stats sd
#' @export
Q.calc_quality <- function(df_obs, df_sim, type = "NSE", min_data = 0.5) {
  
  df_all <- merge(df_sim, df_obs, by = "Date", all.x = TRUE)
  df_all$Sim[is.na(df_all$Value)] <- NA
  
  #check if it is enough data
  if (sum(!is.na(df_all$Sim)) <= min_data * length(df_all$Sim)) {
    return(NULL)
  }
  
  if (type == "QmeanAbs") {
    val <- list("QmeanAbs" = NA)
  } else if (type == "NSE") {
    val <- list("NSE" = NA)
  } else if (type == "logNSE") {
    val <- list("logNSE" = NA)
  } else if (type == "pBias") {
    val <- list("pBias" = NA)
  } else if (type == "KGE") {
    val <- list("KGE" = NA, "b" = NA, "a" = NA, "r" = NA)
  } else if (type == "KGE_2012") {
    val <- list("KGE" = NA, "b" = NA, "a" = NA, "r" = NA)
  } else if (type == "mod_index_of_agreement") {
    val <- list("index_of_agreement" = NA)
  } else if (type == "MAE") {
    val <- list("MAE" = NA)
  }
    
  df_all_diff <- df_all$Value - df_all$Sim
  df_all_diff2 <- df_all_diff^2
  df_all_obsdiff <- df_all$Value - mean(df_all$Value, na.rm = TRUE)
  
  if (type == "QmeanAbs") {
    mean_obs <- mean(df_all$Value, na.rm = TRUE)
    mean_sim <- mean(df_all$Sim, na.rm = TRUE)
    val <- list("QmeanAbs" = abs(mean_obs - mean_sim))
    
  } else if (type == "mod_index_of_agreement") {
    upper_part <- sum(abs(df_all$Sim - df_all$Value))
    lower_part <- sum(abs(df_all$Sim - mean(df_all$Value)) + 
                      abs(df_all$Value - mean(df_all$Value)))
    val <- list("mod_index_of_agreement" = 1 - upper_part / lower_part)
    
  } else if (type == "MAE") {
    val <- list("MAE" = mean(abs(df_all_diff)))
    
  } else if (type == "NSE") {
    nse <- 1 - (mean(df_all_diff2, na.rm = TRUE) /
                  mean(df_all_obsdiff^2, na.rm = TRUE))
    val <- list("NSE" = nse)
    
  } else if (type == "logNSE") {
    obs <- log(df_all$Value + 1)
    sim <- log(df_all$Sim + 1)
    
    df_all_diff <- obs - sim
    df_all_diff2 <- df_all_diff^2
    df_all_obsdiff <- obs - mean(obs, na.rm = TRUE)
    
    lognse <- 1 - (mean(df_all_diff2, na.rm = TRUE) /
                     mean(df_all_obsdiff^2, na.rm = TRUE))
    val <- list("logNSE" = lognse)
    
  } else if (type == "pBias") {
    
    df_all$Value[df_all$Value == 0] <- 0.00000001
    percent_bias <- mean((df_all$Sim - df_all$Value) /
                           df_all$Value, na.rm = TRUE)
    val <- list("pBias" = percent_bias)
    
  }  else if (startsWith(type, "KGE")) {
    
    r <- cor(df_all$Sim[!is.na(df_all$Sim)],
             df_all$Value[!is.na(df_all$Sim)],
             method = "pearson")
    
    a <- mean(df_all$Sim, na.rm = TRUE) /
      mean(df_all$Value, na.rm = TRUE)
    
    b <- sd(df_all$Sim, na.rm = TRUE) /
      sd(df_all$Value, na.rm = TRUE)
    
    if (type == "KGE_2012") {
      b <- (sd(df_all$Sim, na.rm = TRUE) / mean(df_all$Sim, na.rm = TRUE))/
           (sd(df_all$Value, na.rm = TRUE) / mean(df_all$Value, na.rm = TRUE))
    }
    
    kge <- round(1 - sqrt((1 - r)^2 + (a - 1)^2 + (b - 1)^2), 3)
    val <- list("KGE" = kge, "b" = b, "a" = a, "r" = r)
  
    } else {
    print("Type is not specified, chose one of the following:
          QmeanAbs, NSE, logNSE, KGE or SFDCE (from Werkhoven et al. 2008)")
  }
  return(val)
}


################################################################################
#' @title Plotting daily timeseries
#' @description Functions to plot nicely results
#' @param df_obs data.frame of observed daily discharge with
#' ("Date" and "Value" columns)
#' @param df_sim data.frame of simulated daily discharge with
#' ("Date" and "Sim" Columns)
#' @param df_prec data.frame of daily precipitation as basinwide average
#' ("Date" and "prec" Columns)
#' @param from optional information (Date) where to start plot
#' @param to optional information (Date) where to end plot
#' @param show_plot if TRUE than plot is directly shown in Viewer
#' @return ggplot object
#' @importFrom stats cor
#' @importFrom stats sd
#' @import ggplot2
#' @importFrom rlang .data
#' @export
#'
Q.plot_timeseries <- function(df_obs, df_sim, df_prec,
                            from = NULL, to = NULL,
                            show_plot = TRUE) {

  if (is.null(from)) {
    from <- 1
  }
  if (is.null(to)) {
    to <- nrow(df_obs)
  }

  df_obs <- df_obs[from:to, ]
  df_sim <- df_sim[from:to, ]
  df_prec <- df_prec[from:to, ]

  kge_List <- Q.calc_quality(df_obs, df_sim, type = "KGE")
  sub_title = sprintf("KGE: %f.2. - b: %f.2 - a: %f.2 - r: %f.2",
                      kge_List$KGE, kge_List$b, kge_List$a, kge_List$r)


  ymin <- 0
  ymax <- round(max(max(df_obs$Value, na.rm = TRUE),
              max(df_sim$Sim, na.rm = TRUE)) + 0.1 *
                  max(max(df_obs$Value, na.rm = TRUE),
              max(df_sim$Sim, na.rm = TRUE)),
              0)

  df_prec$adj <- ymax - df_prec$prec / max(df_prec$prec) *  (ymax - ymin) / 2

  p <- ggplot() +
    geom_line(data = df_obs,
              mapping = aes(x = .data$Date, y = .data$Value, color = "Qobs")) +
    geom_line(data = df_sim,
              mapping = aes(x = .data$Date, y = .data$Sim, color = "Qsim")) +
    geom_line(data = df_prec,
              mapping = aes(x = .data$Date, y = .data$adj,   color = "Prec"),
              alpha = 0.5) +

    labs(subtitle = sub_title,
         caption = "only days within simulation period and
                    with observation data are considered")   +
    xlab("Dates") +
    ylab("Discharge [mm/day]") +

    scale_color_manual(name = "", values = c("Qobs" = "blue",
                                             "Qsim" = "red",
                                             "Prec" = "black")) +

    scale_y_continuous(limits = c(ymin, ymax), expand = c(0., 0.),
            sec.axis = sec_axis(~ abs((. - ymax) * (2 * max(df_prec$prec)) / ymax),
                        name = "Precipitation [mm/day]")) +

    theme(panel.grid.major = element_line(size = 0.5, color = "grey"),
          panel.grid.minor = element_line(size = 0.5, color = "grey"),
          panel.background = element_rect(fill = "white"),
          panel.spacing = unit(1, "cm"),
          axis.line = element_line(color = "black",
                                   size = 0.5,
                                   linetype = "solid"),
          plot.caption = element_text(),
          plot.title = element_text(),
          plot.subtitle = element_text(),
          plot.margin = margin(2, 4, 2, 2, "cm"),
          plot.background = element_rect(fill = "white"),

          legend.position = c(0.95, 0.95),
          legend.justification = c("right", "top"),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black"),
          legend.spacing.y = unit(0, "mm"),
          legend.spacing.x = unit(0, "mm"),
          legend.key = element_blank())

  if (show_plot == TRUE) {
    print(p)
  }

return(p)

}

################################################################################
#' @title Plotting monthly timeseries
#' @description Function to plot monthly timeseries
#' (aggregation is done function intern)
#' @param df_obs data.frame of observed daily discharge
#' with ("Date" and "Value" columns)
#' @param df_sim data.frame of simulated daily discharge
#' th ("Date" and "Sim" Columns)
#' @return ggplot object
#' @importFrom stats cor
#' @importFrom stats sd
#' @importFrom rlang .data
#' @import dplyr
#' @export
#'
Q.create_monthly_plot <- function(df_obs, df_sim) {

  df_all <- merge(df_sim, df_obs, by = "Date", all.x = TRUE)
  df_all$Sim[is.na(df_all$Value)] <- NA

  sum_qsim <- NULL
  sum_qobs <- NULL
  date <- NULL

  df_all_m <- df_all %>%
    mutate(month = format(.data$Date, "%m"), year = format(.data$Date, "%Y")) %>%
    group_by(.data$month, .data$year) %>%
    summarise(sum_Qsim = sum(.data$Sim), sum_Qobs = sum(.data$Value)) %>%
    mutate(DATE = as.Date(paste("15", .data$month, .data$year, sep = "."),
           format = "%d.%m.%Y"))
  df_all_m <- df_all_m[order(df_all_m$DATE), ]

  df_all_diff <- df_all_m$sum_Qobs - df_all_m$sum_Qsim
  df_all_diff2 <- df_all_diff ^ 2
  df_all_obsdiff <- df_all_m$sum_Qobs - mean(df_all$Value, na.rm = TRUE)

  nse <- 1 - (mean(df_all_diff2, na.rm = TRUE) /
              mean(df_all_m$sum_Qobs^2, na.rm = TRUE))

  b <- sd(df_all_m$sum_Qsim, na.rm = TRUE) /
       sd(df_all_m$sum_Qobs, na.rm = TRUE)

  a <- mean(df_all_m$sum_Qsim, na.rm = TRUE) /
       mean(df_all_m$sum_Qobs, na.rm = TRUE)

  r <- cor(df_all_m$sum_Qsim[!is.na(df_all_m$sum_Qsim)],
           df_all_m$sum_Qobs[!is.na(df_all_m$sum_Qobs)],
           method = "pearson")

  kge <- round(1 - sqrt((1 - r) ^ 2
                + (a - 1) ^ 2
                + (b - 1) ^ 2),
              3)

  ymin <- 0
  ymax <- round(max(max(df_all_m$sum_Qobs, na.rm = TRUE),
                    max(df_all_m$sum_Qsim, na.rm = TRUE))
                        + 0.1 * max(max(df_all_m$sum_Qobs, na.rm = TRUE),
                    max(df_all_m$sum_Qsim, na.rm = TRUE)),
                    0)

  p <- ggplot() +
    geom_line(data = df_all_m,
              mapping = aes(x = date, y = sum_qobs, color = "Qobs")) +
    geom_line(data = df_all_m,
              mapping = aes(x = date, y = sum_qsim, color = "Qsim")) +

    labs(subtitle = sprintf("KGE: %.3f - b: %.3f - a: %.3f - r: %.3f",
                            kge, b, a, r),
         caption = "only days within simulation period
                    and with observation data are considered")   +
    xlab("Dates") +
    ylab("Discharge [mm/month]") +

    scale_color_manual(name = NULL,
                      values = c("Qobs" = "blue",  "Qsim" = "red")) +

    theme(panel.grid.major = element_line(size = 0.5, color = "grey"),
          panel.grid.minor = element_line(size = 0.5, color = "grey"),
          panel.background = element_rect(fill = "white"),
          panel.spacing = unit(1, "cm"),
          axis.line = element_line(color = "black",
                                   size = 0.5,
                                   linetype = "solid"),
          plot.caption = element_text(),
          plot.title = element_text(),
          plot.subtitle = element_text(),
          plot.margin = margin(2, 4, 2, 2, "cm"),
          plot.background = element_rect(fill = "white"),

          legend.position = c(0.95, 0.95),
          legend.justification = c("right", "top"),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black"),
          legend.spacing.y = unit(0, "mm"),
          legend.spacing.x = unit(0, "mm"),
          legend.key = element_blank())
  print(p)

  return(p)
}
