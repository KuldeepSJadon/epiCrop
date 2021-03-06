#' Weather check&interpolation
#'
#' This function check the basic formatting and quality checks of the weather
#' data. It will also infill shorter gaps of temperature and relative humidity series.
#' Longer series impuutation for these variables, as well as rain, should be implemented
#' using multivariate methods. Solar radiation is imputed using \code{nasapower}
#' package.
#' @param data The weather data formated as \code{data frame}.
#' @param report Maximum allowed proportion of missing values. Set to 0.01 by default.
#' @param infill_gap Maximum alowed gap for interpolation of missing values for temperature and
#' relative humidity. Default maximium infill gap is set to 12.
#' @import dplyr
#' @importFrom nasapower get_power
#' @export
#' @keywords interpolation weather
#' @return This function returns a \code{data.frame} including columns:
#' @examples
#' \donttest{
#' library(epiCrop)
#' dt <- epiCrop::weather
#' dt[5,c("temp","rhum", "rain")] <- 1000 #large values
#' WthCheckImpute(dt, report = TRUE)
#' dt <- epiCrop::weather
#' dt[10,c("temp","rhum", "rain")] <- -9999 #negative
#' WthCheckImpute(dt)
#' dt <- epiCrop::weather
#' dt[5,c("temp","rhum", "rain")] <- 1000 #large values
#' WthCheckImpute(dt, report = TRUE)
#' dt <- epiCrop::weather
#' dt[10,c("temp","rhum", "rain")] <- -9999 #negative value
#' #by defauld function returns a list with two data frames
#' out <- WthCheckImpute(dt)
#' head(out[[1]])
#' head(out[[2]])
#'
#' data <- WthCheckImpute(dt,report = FALSE)
#' head(data)
#' }


WthCheckImpute <- function(data,
                           report = TRUE,
                           infill_gap = NULL
                           )
{
  data[["rain"]] -> rain
  data[["rhum"]] -> rh
  data[["temp"]] -> temp
  data[["sol_rad"]] -> sol_rad


  ###############################################
  #Check the format of data
  ###############################################
  cnm <-
    c("short_date", "hour", "lat", "long", "temp", "rhum", "sol_rad")
  if (all(cnm %in% colnames(data)) == FALSE)
    stop("Data format problem: Rename column names.")
  if (length(cnm) != 7)
    stop("Data format problem: There has to be 7 columns in the data.")

  if (length(c(unique(with(data, long)), unique(with(data, lat)))) > 2)
    stop(
      "Data format problem: Different latitudes/longitudes. Data should be provided for a single location"
    )


  if (any(apply(data[, c("hour", "lat", "long", "temp", "rhum", "sol_rad")], 2,
                is.numeric)) == FALSE) {
    stop(
      "Longitude, latitude, temperature, relative humidty, solar radiation must be numeric variables!"
    )
  }

  if (nrow(data) / base::unique(data[, "short_date"]) %>% length() != 24) {
    stop("The weather data needs to have 24 rows per day!")
  }


  if (lubridate::is.Date(data[, "short_date"]) == FALSE) {
    data[, "short_date"] <-
      base::as.Date(as.character(data[, "short_date"]),  "%Y-%m-%d")
  }

  ###############################################
  #Check for extreme values
  ###############################################
  if (any(rh < 0, na.rm = TRUE))
    stop("Negative relative humidity values are not possible!")
  if (any(sol_rad < 0, na.rm = TRUE))
    stop("Negative solar radiation values are not possible!")
  if (any(rh > 100, na.rm = TRUE))
    stop("Relative humidity values could not be higher than 100%!")
  if (any(rain < 0, na.rm = TRUE))
    stop("Negative rain values are not possible!")




  ###############################################
  #Check data before imputation
  ###############################################
  na_ls <- list()
  #Missing data treatment
    for (z in seq(c("temp", "rhum", "rain", "sol_rad"))) {
      # z = "temp"
      i <- c("temp", "rhum", "rain", "sol_rad")[z]
      x <- data[[i]]
      NAs <- sum(is.na(x))
      percent <- round(NAs / length(x) * 100, 2)
      criteria <- as.numeric(is.na(x))
      crit <-
        stats::ave(is.na(x), cumsum(is.na(x) == 0), FUN = cumsum)

      for (y in seq(crit[2:length(crit)])) {
        if (crit[y] < crit[y + 1])
          crit[y] <- 0
      }
      crit <- crit[crit > 0]
      no_gaps <- length(crit)

      if (no_gaps > 0) {
        mean_gap <- mean(crit)
        max_gap <- max(crit)
      } else{
        mean_gap <- 0
        max_gap <- 0
      }
q <-
    quantile(x, probs = c(.0001, .9999), na.rm = TRUE)

      na_ls[[z]] <-
        data.frame(
          data = "raw",
          variable = as.character(i),
          less0.01perc = q[[1]],
          more99.99perc= q[[2]],
          percent = percent,
          no_gaps = no_gaps,
          mean_gap = mean_gap,
          max_gap = max_gap,
          stringsAsFactors = FALSE
        )

    }
    na_df_before <-
      dplyr::bind_rows(na_ls)
    rm(q, na_ls)


  ###############################################
  #Infill solar radiation
  ###############################################

  if(sum(is.na(sol_rad)) / length(sol_rad) > .9){
    warning(
      "Solar radiation will be infiled using nasapower package! Active interent connection is necessary. The total daily sum of solar radiation in MJ/m2 values are assigned to 12th hour of each day"
    )
    dff <-
      nasapower::get_power(
        community = "AG",
        lonlat = c(data[1, "long"], data[1, "lat"]),
        pars = c("ALLSKY_SFC_SW_DWN"),
        dates = c(data[1, "short_date"], data[nrow(data), "short_date"]),
        temporal_average = "DAILY"
      ) %>%
      select(c("YYYYMMDD", "ALLSKY_SFC_SW_DWN"))
    colnames(dff) <-  c("short_date", "sol_nasa")
    dff$hour <-  12
    data <-
      left_join(data, dff, by = c("short_date", "hour"))
    data[is.na(data$sol_rad),"sol_rad"] <- 0
  }

if(is.null(infill_gap))infill_gap <- 11

  if (sum(is.na(with(data, rain, temp, rhum))) > 0) {
    temp <-
      round(zoo::na.spline(temp, na.rm = FALSE, maxgap = infill_gap), 1)
    rh <-
      round(zoo::na.spline(rh, na.rm = FALSE, maxgap = infill_gap), 0)
    rh  <- sapply(rh, function(x)
      ifelse(x > 100, x <- 100, x))
  }



  ###############################################
  #Check data before imputation
  ###############################################
  na_ls <- list()

  #Missing data treatment

    for (z in seq(c("temp", "rhum", "rain", "sol_rad"))) {
      # z = "temp"
      i <- c("temp", "rhum", "rain", "sol_rad")[z]
      x <- data[[i]]
      NAs <- sum(is.na(x))
      percent <- round(NAs / length(x) * 100, 2)
      criteria <- as.numeric(is.na(x))
      crit <-
        stats::ave(is.na(x), cumsum(is.na(x) == 0), FUN = cumsum)

      for (y in seq(crit[2:length(crit)])) {
        if (crit[y] < crit[y + 1])
          crit[y] <- 0
      }
      crit <- crit[crit > 0]
      no_gaps <- length(crit)

      if (no_gaps > 0) {
        mean_gap <- mean(crit)
        max_gap <- max(crit)
      } else{
        mean_gap <- 0
        max_gap <- 0
      }
      q <-
        quantile(x, probs = c(.0001, .9999), na.rm = TRUE)

      na_ls[[z]] <-
        data.frame(
          data = "infilled",
          variable = as.character(i),
          less0.01perc = q[[1]],
          more99.99perc= q[[2]],
          percent = percent,
          no_gaps = no_gaps,
          mean_gap = mean_gap,
          max_gap = max_gap,
          stringsAsFactors = FALSE
        )

    }
    na_df_after <-
      dplyr::bind_rows(na_ls)
    rm(na_ls)


  na_df <- rbind(na_df_before, na_df_after)

  data[["rain"]] <- rain
  data[["rhum"]] <- rh
  data[["temp"]] <- temp



  if (report == TRUE)
    return(list(data, na_df))
  if (report == FALSE)
    return(data)
}


