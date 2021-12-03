#library(ggplot2)

#' @title Reading GRDC data
#' @description Functions to read discharge data
#' @param basinObject_id grdc_number of basin, can be obtained from basinObject
#' @param basinObject_DataDir dataDirection as string can be obtained from basinObject
#' @param basinObject_cont continent as string (au, af, as, na, eu, sa)
#' @param start start data as date to read GRDC data
#' @param end end date as date to read GRDC data
#' @param useFolder if defined another folder than data/calibration/cont can be set to look for discharge data (GRDC-like data)
#' @return dataframe with Date and Q as given in GRDC-file with name "(grdc_number)_Q_Day.Cmd.txt"
#' @importFrom utils read.csv2
#' @export

Q.readGRDC <- function(basinObject_id, basinObject_DataDir, basinObject_cont,  start, end, useFolder=NULL){

  start <- as.Date(start, "%d.%m.%Y")
  end <- as.Date(end, "%d.%m.%Y")
  folder <- file.path(basinObject_DataDir, "calibration", basinObject_cont)

  if (!is.null(useFolder)){
    folder <- useFolder
  }

  path2read <- file.path(folder, paste0(as.character(basinObject_id), "_Q_Day.Cmd.txt"))
  data <- read.csv2(path2read, skip=37, sep=";", header=F, dec=".", colClasses=c("Date",NA, "numeric"))
  colnames(data) <- c("Date", "Time","Value")

  timeseq = data.frame("Date"=seq(start, end, by=1))
  dataFill <- merge(timeseq, data, by="Date", all.x=T)
  dataFill$Value[dataFill$Value < 0] <- NA


  start_new <- max(min(data[["Date"]]), start)
  end_new   <- min(max(data[["Date"]]), end)
  countNA <- length(which(is.na(dataFill[["Value"]])))

  #checking ig everyhting is fined and informing user if there are NAs in timeseries
  if (countNA>0) { warning("NoData Values (< 0) are filled with NA")}
  if (start_new != start) { warning(paste("Read timeseries is to short, it starts at", start_new, "NA introduced"))}
  if (end_new != end) { warning(paste("Read timeseries is to short, it ends at", end_new,"NA introduced"))}

  return(dataFill)
}

###############################################################################################################################
#' @title m³/s to mm/d
#' @description Functions to convert m³/s to mm/d
#' @param timeseries vector/matrix of data that needs to be converted 
#' @param area GAREA information of basin, can be obtained grom GAREA of basinObject
#' @return changed timeseries
#' @export
#' 
Q.convert_m3s_mmday <- function(timeseries, area){

  timeseries = timeseries*60*60*24/area/1000 #area must be in km²!

  return(timeseries)

}

###############################################################################################################################
#' @title mm/d to m³/s
#' @description Functions to convertmm/d to m³/s
#' @param timeseries vector/matrix of data that needs to be converted 
#' @param area GAREA information of basin, can be obtained grom GAREA of basinObject
#' @return changed timeseries
#' @export
Q.convert_mmday_m3s <- function(timeseries, area){

  timeseries = timeseries/(60*60*24)*area*1000 #area must be in km²!

  return(timeseries)
}

###############################################################################################################################
#' @title Calculating Signature Indices
#' @description Function to calculate several Signature Indices from simulated flow
#' @param df dataframe of simulated discharge with (at least) 'Date' and 'Sim' column
#' @param Type string (FDC.Slope, ...) \cr
#' \itemize{
#'   \item FDC.Slope: flow duration curve slope calculated as described in Kapangaziwiri et al. 2012
#'   \item maxTiming: mean julian date of each annual 1-day maximum (Richter et al. 1996)
#'   \item minTiming: mean julian date of each annual 7-day minimum (modified, based on Richter et al. 1996)
#' }
#' @return value (as float)
#' @importFrom stats cor
#' @importFrom stats sd 
#' @export
#'  
Q.calcSI <- function(df, Type="FDC.Slope"){
  
  if (Type == "FDC.Slope"){
    
    Q90 <- as.numeric(quantile(df$Sim, 0.1))
    if (Q90 == 0){
      Q90 <- min(df$Sim[df$Sim > 0]) #replacement with min != 0
    }
    Q10 <- as.numeric(quantile(df$Sim, 0.9))
    val <- (log(Q90) - log(Q10)) / 80
    
  } else  if (Type == "maxTiming"){
   
    df$year <- format(df$Date, "%Y")
    minYear <- min(df$year); maxYear <- max(df$year)
    DOY <- lapply(minYear:maxYear, function(x) { which(df$Sim[df$year==x] == max(df$Sim[df$year==x]))} )  
    val <- mean(unlist(DOY))
   
   } else  if (Type == "minTiming"){
     
     ma <- function(x, n = 7){ stats::filter(x, rep(1 / n, n), sides = 2)}
     
     df$rollmean <- ma(df$Sim)
     df$year <- format(df$Date, "%Y")
     minYear <- min(df$year); maxYear <- max(df$year)
     DOY <- lapply(minYear:maxYear, function(x) { which(df$rollmean[df$year==x] == min(df$rollmean[df$year==x], na.rm=T) )} )  
     val <- mean(unlist(DOY))
     
  } else {
    stop('Type is not defined. Please chose between the options as defined in the description of the function!')
  }
  
  return(val)

}
###############################################################################################################################
#' @title Calculating Benchmark
#' @description Function to calculate several benchmarks to compare simulated and observed discharge. NA or negative values in observed data are omitted.
#' @param df_obs data.frame of observed discharge with "Date" and "Value" columns
#' @param df_sim data.frame of simulated discharge with "Date" and "Sim" Columns
#' @param Type string (QmeanAbs, NSE, logNSE, KGE or pBias)
#' \itemize{
#'   \item QmeanAbs: absolute differenc between mean values of sim and obs
#'   \item NSE: Nash-Sutcliff-Efficiency
#'   \item logNSE: NSE of logarithmic values 
#'   \item KGE: Kling-Gupta-efficiency and its components
#'   \item pBias:  relative bias as described in van Werkhoven et al. 2008
#'   \item deltaSD: not implemented yet! 
#' }
#' @param minData minimum on available observed data in examined timeperiod, \cr 
#' e.g. 0.5 for 50 percent (default)
#' @return named list with benchmarks
#' @importFrom stats cor
#' @importFrom stats sd 
#' @export
Q.calcQuality <- function(df_obs, df_sim, Type="NSE", minData=0.5){

  df_all <- merge(df_sim, df_obs, by="Date", all.x=T)
  df_all$Sim[is.na(df_all$Value)] <- NA

  #check if it is enough data
  if (sum(is.na(df_all$Sim)) > minData * length(df_all$Sim)) {
    if (Type == "QmeanAbs"){
      val <- list("QmeanAbs" = NA)
    } else if (Type == "NSE"){
      val <- list("NSE" = NA)
    } else if (Type == "logNSE"){
      val = list("logNSE"=NA)
    } else if (Type == "pBias") {
      val = list("pBias"=NA)
    } else if (Type=="KGE"){
      val = list("KGE"=NA, "b"=NA, "a"=NA, "r"=NA)
    }
  }
  
  df_all_diff <- df_all$Value - df_all$Sim
  df_all_diff2 <- df_all_diff^2
  df_all_obsdiff <- df_all$Value - mean(df_all$Value, na.rm=T)

  if (Type == "QmeanAbs"){
    meanObs <- mean(df_all$Value, na.rm=T)
    meanSim <- mean(df_all$Sim, na.rm=T)
    val <- list("QmeanAbs" = abs(meanObs - meanSim))

  } else if (Type == "NSE"){
    NSE <- 1 - (mean(df_all_diff2, na.rm=T) / mean(df_all_obsdiff^2, na.rm=T))
    val = list("NSE"=NSE)

  } else if (Type == "logNSE"){
    obs <- log(df_all$Value + 1)
    sim <- log(df_all$Sim + 1)

    df_all_diff <- obs - sim
    df_all_diff2 <- df_all_diff^2
    df_all_obsdiff <- obs - mean(obs, na.rm=T)

    NSE <- 1 - (mean(df_all_diff2, na.rm=T) / mean(df_all_obsdiff^2, na.rm=T))
    val = list("logNSE"=NSE)

  } else if (Type == "pBias"){
    
    #df_all$diff <- df_all$Sim - df_all$Value
    #bias <- mean(df_all$diff, na.rm=T)
    df_all$Value[df_all$Value == 0] <- 0.00000001
    pBias <- mean((df_all$Sim - df_all$Value)/df_all$Value, na.rm=T)
    
    val = list("pBias"=pBias)
   
  }  else if (Type == "KGE"){
    b = sd(df_all$Sim, na.rm=T)/sd(df_all$Value, na.rm=T)
    a = mean(df_all$Sim, na.rm=T)/mean(df_all$Value, na.rm=T)
    r = cor(df_all$Sim[!is.na(df_all$Sim)], df_all$Value[!is.na(df_all$Sim)], method ="pearson")
    KGE = round(1- sqrt((1-r)^2+(a-1)^2+(b-1)^2),3)

    val = list("KGE"=KGE, "b"=b, "a"=a, "r"=r)

  } else {
    print("Type is not specified, chose one of the following: QmeanAbs, NSE, logNSE, KGE or SFDCE (from Werkhoven et al. 2008)")
  }
  # SFDCE = as.numeric( abs( ( (stats::quantile(sim, 0.7, na.rm=T) - stats::quantile(sim, 0.3, na.rm=T) ) / 40) -
  #                ( (stats::quantile(obs, 0.7, na.rm=T) - stats::quantile(obs, 0.3, na.rm=T) ) / 40) ) )
  # #wenn alle Werte +x sind, dann liefert SFDCE trotzdem gut Ergebnisse, da nur Unterschied betrachtet wird zwischen Q30 und Q70
  return(val)
}

###############################################################################################################################
#' @title Plotting daily timeseries
#' @description Functions to plot nicely results
#' @param df_obs data.frame of observed daily discharge with ("Date" and "Value" columns)
#' @param df_sim data.frame of simulated daily discharge with ("Date" and "Sim" Columns)
#' @param df_prec data.frame of daily precipitation as basinwide average ("Date" and "prec" Columns)
#' @param from optional information (Date) where to start plot
#' @param to optional information (Date) where to end plot
#' @param showPlot if TRUE than plot is directly shown in Viewer, inf FALSE than not
#' @return ggplot object 
#' @importFrom stats cor
#' @importFrom stats sd 
#' @import ggplot2
#' @importFrom rlang .data
#' @export
#' 
Q.plotTimeseries <- function(df_obs, df_sim, df_prec, from=NULL, to=NULL, showPlot=T){

  if (is.null(from)) { from=1 }
  if (is.null(to)  ) { to=nrow(df_obs)  }

  df_obs <- df_obs[from:to,]
  df_sim <- df_sim[from:to,]
  df_prec <- df_prec[from:to,]

  KGE_List <- Q.calcQuality(df_obs, df_sim, Type="KGE")

  ymin = 0
  ymax = round(max(max(df_obs$Value, na.rm=T), max(df_sim$Sim, na.rm=T)) + 0.1 * max(max(df_obs$Value, na.rm=T), max(df_sim$Sim, na.rm=T)),0)

  df_prec$adj = ymax - df_prec$prec/max(df_prec$prec) *  (ymax - ymin)/2

  p <- ggplot() +
    geom_line(data = df_obs, aes(x =  .data$Date, y = .data$Value, color="Qobs")) +
    geom_line(data = df_sim, aes(x = .data$Date, y = .data$Sim,    color="Qsim")) +
    geom_line(data = df_prec, aes(x = .data$Date, y = .data$adj,   color="Prec"), alpha=0.5) +
    #geom_col fpr precipitation
    labs(subtitle = paste0("KGE:",as.character(KGE_List[[1]]), "   b:",as.character(KGE_List[[2]]),
                           "   a:",as.character(KGE_List[[3]]), "   r:",as.character(KGE_List[[4]])),
         caption = "only days within simulation period and with observation data are considered")   +
    xlab('Dates') +
    ylab('Discharge [mm/day]') +

    scale_color_manual(name="", values=c("Qobs"="blue",  "Qsim"="red", "Prec"="black")) +

    scale_y_continuous(limits=c(ymin,ymax), expand=c(0.,0.),
                       sec.axis=sec_axis(~ abs((.-ymax) *(2*max(df_prec$prec))/ymax) , name="Precipitation [mm/day]")) +

    theme(panel.grid.major = element_line(size=0.5, color="grey"),
          panel.grid.minor = element_line(size=0.5, color="grey"),
          panel.background = element_rect(fill="white"),
          panel.spacing = unit(1,"cm"),
          axis.line = element_line(color="black", size=0.5, linetype="solid"),
          plot.caption = element_text(),
          plot.title = element_text(),
          plot.subtitle = element_text(),
          plot.margin = margin(2,4,2,2, "cm"),
          plot.background = element_rect(fill="white"),

          legend.position = c(0.95, 0.95),
          legend.justification = c("right", "top"),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black"),
          legend.spacing.y = unit(0, "mm"),
          legend.spacing.x = unit(0, "mm"),
          legend.key=element_blank())

  if (showPlot == T) { print(p) }

return(p)

}

###############################################################################################################################
#' @title Plotting monthly timeseries
#' @description Function to plot monthly timeseries (aggregation is done function intern)
#' @param df_obs data.frame of observed daily discharge with ("Date" and "Value" columns)
#' @param df_sim data.frame of simulated daily discharge with ("Date" and "Sim" Columns)
#' @return ggplot object
#' @importFrom stats cor
#' @importFrom stats sd 
#' @importFrom rlang .data
#' @import dplyr
#' @export
#' 
Q.createMonthlyPlot <- function(df_obs, df_sim){

  df_all <- merge(df_sim, df_obs, by="Date", all.x=T)
  df_all$Sim[is.na(df_all$Value)] <- NA

  sum_Qsim <- NULL; sum_Qobs <- NULL; DATE <- NULL #hack to eliminiate note
  df_all_m <- df_all %>%
    mutate(month = format(.data$Date, "%m"), year=format(.data$Date, "%Y")) %>%
    group_by(.data$month,.data$year) %>%
    summarise(sum_Qsim=sum(.data$Sim), sum_Qobs=sum(.data$Value)) %>%
    mutate(DATE=as.Date(paste("15", .data$month, .data$year, sep="."), format="%d.%m.%Y"))
  df_all_m <- df_all_m[order(df_all_m$DATE),]

  df_all_diff <- df_all_m$sum_Qobs - df_all_m$sum_Qsim
  df_all_diff2 <- df_all_diff^2
  df_all_obsdiff <- df_all_m$sum_Qobs - mean(df_all$Value, na.rm=T)

  NSE <- 1 - (mean(df_all_diff2, na.rm=T) / mean(df_all_m$sum_Qobs^2, na.rm=T))

  b = sd(df_all_m$sum_Qsim, na.rm=T)/sd(df_all_m$sum_Qobs, na.rm=T)
  a = mean(df_all_m$sum_Qsim, na.rm=T)/mean(df_all_m$sum_Qobs, na.rm=T)
  r = cor(df_all_m$sum_Qsim[!is.na(df_all_m$sum_Qsim)], df_all_m$sum_Qobs[!is.na(df_all_m$sum_Qobs)], method ="pearson")
  KGE = round(1- sqrt((1-r)^2+(a-1)^2+(b-1)^2),3)


  ymin = 0
  ymax = round(max(max(df_all_m$sum_Qobs, na.rm=T), max(df_all_m$sum_Qsim, na.rm=T)) + 0.1 * max(max(df_all_m$sum_Qobs, na.rm=T), max(df_all_m$sum_Qsim,na.rm=T)),0)

  p <- ggplot() +
    geom_line(data = df_all_m, aes(x = DATE, y = sum_Qobs, color="Qobs")) +
    geom_line(data = df_all_m, aes(x = DATE, y = sum_Qsim, color="Qsim")) +
    #geom_col fpr precipitation
    labs(subtitle = paste0("NSE:",as.character(round(NSE,3)), "   b:",as.character(round(b,3)),
                           "   a:",as.character(round(a,3)), "   r:",as.character(round(r,3))),
         caption = "only days within simulation period and with observation data are considered")   +
    xlab('Dates') +
    ylab('Discharge [mm/month]') +

    scale_color_manual(name=NULL, values=c("Qobs"="blue",  "Qsim"="red")) +


    theme(panel.grid.major = element_line(size=0.5, color="grey"),
          panel.grid.minor = element_line(size=0.5, color="grey"),
          panel.background = element_rect(fill="white"),
          panel.spacing = unit(1,"cm"),
          axis.line = element_line(color="black", size=0.5, linetype="solid"),
          plot.caption = element_text(),
          plot.title = element_text(),
          plot.subtitle = element_text(),
          plot.margin = margin(2,4,2,2, "cm"),
          plot.background = element_rect(fill="white"),

          legend.position = c(0.95, 0.95),
          legend.justification = c("right", "top"),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black"),
          legend.spacing.y = unit(0, "mm"),
          legend.spacing.x = unit(0, "mm"),
          legend.key=element_blank())
  print(p)

  return(p)

}

