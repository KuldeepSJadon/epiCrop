#' Weather interpolation
#'
#' This function calculates potato late blight risk using BlightR model.
#'
#' @param data The weather data formated as \code{data frame}.
#' @param max_na Maximum allowed proportion of missing values. Set to 0.01 by default.
#' @import dplyr
#' @importFrom nasapower get_power
#' @export
#' @keywords interpolation weather
#' @return This function returns a \code{data.frame} including columns:
#' @examples
#' \donttest{
#' library(epiCrop)
#' weather <- system.file("data/blightR_weather.csv", package="epiCrop")
#' out <- system.time(BlightR(weather))
#' head(out)
#' }



head(data)
data <- weather
data[666:674,c("temp","rhum")] <- NA #9 NAs
data[766:775,c("temp","rhum")] <- NA #10 NAs
data[866:876,c("temp","rhum")] <- NA #11 NAs

data[, "sol_rad"] <- NA

data[5,c("temp","rhum", "rain")] <- 1000 #large values
data[10,c("temp","rhum", "rain")] <- -9999 #negative


data[["rain"]] -> rain
data[["rhum"]] -> rh
data[[ "temp"]] -> temp
data[[ "sol_rad"]] -> sol_rad


#Check the format of the data frame
cnm <- c("short_date","hour", "lat", "long","temp", "rhum", "sol_rad")
if(all(cnm %in% colnames(data))==FALSE) stop("Rename column names."); rm(cnm)

if(any(apply(data[ , c("temp", "rhum", "sol_rad")],2, is.numeric))==FALSE ){
  stop("Temperature, relative humidty, solar radiation need to be numeric variables!")
}

# Sort columns
if(!"doy"%in% colnames(data)) data$doy <- lubridate::yday(data$short_date)

if(lubridate::is.Date(data[ , "short_date"]) == FALSE){
  data[ , "short_date"]<-
    base::as.Date( as.character(data[ , "short_date"]),  "%Y-%m-%d")
}

if(nrow(data)/base::unique(data[ , "short_date"]) %>% length()!=24){
  stop("The weather data needs to have 24 rows per day!")
}



if(sum(is.na(sol_rad))/length(sol_rad)>.9){
  warning("Solar radiation will be infiled using nasapower package! Active interent connection is necessary. The total daily sum of solar radiation in MJ/m2 values are assigned to 12th hour of each day")
    dff <-
      nasapower::get_power(community = "AG",
                           lonlat = c(data[1, "long"], data[1, "lat"]),
                           pars = c("ALLSKY_SFC_SW_DWN"),
                           dates = c(data[1, "short_date"],data[nrow(data), "short_date"]),
                           temporal_average = "DAILY") %>%
      select(c("YYYYMMDD","ALLSKY_SFC_SW_DWN"))
    colnames(dff) <-  c("short_date", "sol_nasa")
    dff$hour <-  12
    data <-
    left_join(data, dff, by = c("short_date", "hour"))


}




if(any(rh<0, na.rm = TRUE))print("Negative relative humidity values are not possible!")
if(any(sol_rad<0, na.rm = TRUE))print("Negative solar radiation values are not possible!")
if(any(rh>100, na.rm = TRUE))print("Relative humidity values could not be higher than 100%!")
if(any(rain<0, na.rm = TRUE))print("Negative rain values are not possible!")



if(any(data[,c("temp","rhum", "rain")]))
lapply( data[,c("temp","rhum", "rain", "sol_rad")],
        function(x)quantile(x, probs=c(.0001, .9999), na.rm = TRUE)) %>% bind_cols()

quantile(temp, probs=c(.0001, .9999), na.rm = TRUE)


if(any(is.na(data))){
  # na_sum <-
    base::summary(data) %>%
    base::as.data.frame() %>%
    dplyr::filter(.,grepl("NA", Freq)) %>%
    select(-Var1) %>%
    rename( Variable =Var2, NAs = Freq ) %>%
    mutate(NAs = as.numeric(regmatches(NAs,gregexpr("\\d+",NAs))[[1]])) %>%
    mutate("Percent[%]"= round(NAs/nrow(data)*100,2))


  print(na_sum)
  warning("Data has missing values!")
}



if (is.null(infill_gap)) {
  infill_gap <- 7
}

if (sum(is.na(with(weather, rain, temp, rhum))) > 0) {
  temp <-
    round(zoo::na.spline(temp, na.rm = FALSE, maxgap = infill_gap), 1)
  rh <-
    round(zoo::na.spline(rh, na.rm = FALSE, maxgap = infill_gap), 0)
  rh  <- sapply(rh, function(x)
    ifelse(x > 100, x <- 100, x))
}



if(plot_hist == TRUE){

}
