#' Irish Rules
#'
#' This function calculates potato late blight risk using Modified Irish Ruels model
#' (Cucak 2019) and the orginal version as reported by Bourke (1953).
#' @param data The weather data in formated as data frame
#' @param model_parameters Set of parameters to be used for model run. The default setting
#'  is to use modified version of the model. It is also possible to run the original version of the model.
#'  It is also possible to pass custom parameters for the opurpose of model calibration.
#' @param temporal_res By default, the teporal resolution of the output is daily. The output is a data frame
#' with dates and the estimated risk for the given day. By changing the argument \code{temporal_res = "hourly"}
#' The output will be returned in an hourly resolution.
#' @keywords Irish Rules, potato late blight, crop disease forecasting
#' @import dplyr
#' @export
#' @return
#' If the desired output resolution is \code{"hourly"} This function returns a \code{data.frame}
#' including columns:
#' \itemize{
#'  \item \code{"date"}:  Date formated as "yyyy-mm-dd"
#'  \item \code{"ir"}: Daily maximum risk.
#' }
#'
#' \itemize{
#'  \item \code{"date"}:  Date formated as "yyyy-mm-dd"
#'  \item \code{"hour"}: Hour of the day.
#'  \item ir Hourly risk.
#' }
#' @examples
#' \donttest{
#' library(epiCrop)
#' data <- system.file("data/blightR_weather.csv", package="epiCrop")
#' #Daily output
#' out <- system.time(IrishRulesModel(data))
#' head(out)
#' #Hourly output
#' out <- system.time(IrishRulesModel(data, temporal_res = "hourly"))
#' head(out)
#' }

IrishRules <- function(data,
                       model_parameters = "modified",
                       temporal_res = "daily"
                       ) {
  wet_before <- 3
  wet_after <- 3

  # Parameter list
  if (model_parameters == "modified") {
    rh_thresh <- 88
    temp_thresh <- 10
    hours <- 10   #sum of hours before EBH accumulation
    lw_rhum <- "rainrh"
    lw_rhum_threshold <- 90#threshold for estimation of leaf wetness using relative humidity
  }else if (model_parameters == "default") {
    rh_thresh <- 90
    temp_thresh <- 10
    hours <- 12
    lw_rhum <- "rainrh"
    lw_rhum_threshold <- 90#threshold for estimation of leaf wetness using relative humidity
  } else if (model_parameters == "custom") {
    if(!is.data.frame(model_parameters)){
      stop("Custom parameters must be provided as a data frame.
           Each column name is one parameter and the fist row contains the parameter value.
           See an example at:https://mladencucak.github.io/AnalysisPLBIreland/Analysis.html#the_analysis")
    }
    #pass a vector of parameters
    rh_thresh <- as.numeric(model_parameters[2])
    temp_thresh <- as.numeric(model_parameters[3])
    hours <- as.numeric(model_parameters[4])
    lw_rhum <- model_parameters[5]  #if is NA then only rain data will be used
    lw_rhum_threshold <- 90#threshold for estimation of leaf wetness using relative humidity

  }

  if(any(is.na(data))){
    na_sum <-
      base::summary(data) %>%
      base::as.data.frame() %>%
      dplyr::filter(.,grepl("NA", Freq)) %>%
      select(-Var1) %>%
      rename( Variable =Var2, NAs = Freq ) %>%
      mutate(NAs = as.numeric(regmatches(NAs,gregexpr("\\d+",NAs))[[1]])) %>%
      mutate("Percent[%]"= round(NAs/nrow(data)*100,2))
    warning("Data has missing values!")
    print(na_sum)

  }


  data[["rain"]] -> rain
  data[["rhum"]] -> rh
  data[[ "temp"]] -> temp



  if (sum(is.na( c( temp, rh))) > 0) {
    stop(print("The sum of NAs is more than 7! Check your weather data."))
  }

  # "Out of boounds"
  rain <- c(rain, rep(0, 20))
  temp <- c(temp, rep(0, 20))
  rh <- c(rh, rep(0, 20))

  # conditions for sporulation
  criteria <- as.numeric(temp >= temp_thresh & rh >= rh_thresh)

  # cumulative sum of hours that meet the criteria for sporulatoion with restart at zero
  criteria_sum <-
    stats::ave(criteria, cumsum(criteria == 0), FUN = cumsum)

  # Initiate risk accumulation vector
  risk <- rep(0, length(temp))

  criteria_met12  <-
    as.numeric(criteria_sum >= hours) #accumulation of EBH starts after sporulation
  idx             <- which(criteria_sum == hours)

  #If there are no accumulations return vector with zeros
  if (sum(criteria_sum == hours) == 0) {
    #breaks the loop if there is no initial accumulation of 12 hours
    if(temporal_res == "daily"){
      final <-tapply(head(risk, -20), data$doy, max)
    }
    if(temporal_res == "hourly"){
      final <- head(risk,-20) #remove last 20 values that were added to vectors to prevent "Out of bounds" issue
    }
    return(final)
  } else{
    for (j in 1:length(idx)) {
      #switch that looks if there was wetness: first rain, then both rain and rh, if rh exists
      if (if (lw_rhum == "rain") {
        #if only rain
        (sum(rain[(idx[j] - wet_before):(idx[j] + wet_after)]) >= 0.1)           #just see rain sum
      } else{
        #If there is no rain data just consider rh threshold
        rain_missing <-
          rain[(idx[j] - wet_before):(idx[j] + wet_after)]
        if (any(is.na(rain_missing))) {
          any((any(rh[(idx[j] - wet_before):(idx[j] + wet_after)] >= lw_rhum_threshold)))
        } else {
          any((any(rh[(idx[j] - wet_before):(idx[j] + wet_after)] >= lw_rhum_threshold)) |
              #take both as possible switches
              (sum(rain[(idx[j] - wet_before):(idx[j] + wet_after)]) >= 0.1))
        }


      })
      # outputs true or false
      {
        n <- idx[j]        #start accumulation from 12th hour
      } else {
        n <- idx[j] + 4      #start accumulation from 16th hour
      }
      s <- criteria_met12[n]

      # if a break of less than or equal to 5 hours
      m <- n - 1

      while (s == 1)
      {
        risk[n] <- risk[m] + 1
        n <- n + 1
        m <- n - 1
        s <- criteria[n]
        if (s == 0 && (criteria[n + 2] == 1)) {
          n = n + 2
          s = 1
        } else if (s == 0 && (criteria[n + 3] == 1)) {
          n = n + 3
          s = 1
        } else if (s == 0 && (criteria[n + 4] == 1)) {
          n = n + 4
          s = 1
        } else if (s == 0 && (criteria[n + 5] == 1)) {
          n = n + 5
          s = 1
        }
      }

    }

    if(temporal_res == "daily"){
      if(!"doy"%in% colnames(data)) data$doy <- lubridate::yday(data$short_date)
      final <-tapply(head(risk, -20), data$short_date, max) %>% as.data.frame()
    }
    if(temporal_res == "hourly"){
      data[["ir"]]<- head(risk,-20) #remove last 20 values that were added to vectors to prevent "Out of bounds" issue
      final <- data[, c("short_date", "hour", "ir")]
    }




  }
  return(final)

}
