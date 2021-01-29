preprocess_data <- function(data){

  chk::chk_superset(names(data), c("Year", "Month", "Day",
                                   "Station", "Units", "Variable",
                                   "Value"), x_name = "Column names in uploaded data")

  data$Year %<>% as.integer()
  data$Month %<>% as.integer()
  data$Day %<>% as.integer()

  data$Station %<>% as.character()
  data$Units %<>% as.character()
  data$Variable %<>% as.character()

  data$Value %<>% as.numeric()

  if("Second" %in% names(data)){
    data$Second %<>% as.integer()
  }
  if("Minute" %in% names(data)){
    data$Minute %<>% as.integer()
  }
  if("Hour" %in% names(data)){
    data$Hour %<>% as.integer()
  }
  if("DetectionLimit" %in% names(data)){
    data$DetectionLimit %<>% as.numeric()
  }
  if("ResultLetter" %in% names(data)){
    data$ResultLetter %<>% as.character()
  }
  data
}

check_data_upload <- function(data){

  data <- preprocess_data(data)

  names <- c("Station", "Variable", "Value",
             "Units", "Year", "Month", "Day")

  if(!all(names %in% names(data))){
    chk::err("Uploaded data must have column names: ",
             err::cc(names, conjunction = "and",
                     ellipsis = 1000))
  }

  check_values(data$Year, c(1900L, 2100L), x_name = "Column 'Year'")
  check_values(data$Month, c(1L, 12L), x_name = "Column 'Month'")
  check_values(data$Day, c(1L, 31L), x_name = "Column 'Day'")

  chk_not_any_na(data$Station, x_name = "Column 'Station'")
  chk_not_any_na(data$Value, x_name = "Column 'Station'")
  chk_not_any_na(data$Variable, x_name = "Column 'Station'")

  if("Second" %in% names(data)){
    check_values(data$Second, c(0L, 59L, NA), x_name = "Column 'Second'")
  }
  if("Minute" %in% names(data)){
    check_values(data$Minute, c(0L, 59L, NA), x_name = "Column 'Minute'")
  }
  if("Hour" %in% names(data)){
    check_values(data$Hour, c(0L, 23L, NA), x_name = "Column 'Hour'")
  }

  return(data)
}

preprocess_data_upload <- function(data){
  return(data)
}

process_data_upload <- function(data, variable, site, date_range){
  data <- dplyr::filter(data,
                        .data$Variable == variable,
                        .data$Station %in% site,
                        as.Date(.data$DateTime) >= date_range[1],
                        as.Date(.data$DateTime) <= date_range[2])
  return(data)
}

process_dates <- function(data){
  hours <- 0L
  if("Hour" %in% names(data)){
    hours <- ifelse(is.na(data$Hour), 0, data$Hour)
  }
  minutes <- 0L
  if("Minute" %in% names(data)){
    minutes <- ifelse(is.na(data$Minute), 0, data$Minute)
  }
  seconds <- 0L
  if("Second" %in% names(data)){
    seconds <- ifelse(is.na(data$Second), 0, data$Second)
  }
  data %>%
    dplyr::mutate(
      Hour = hours,
      Minute = minutes,
      Second = seconds,
      DateTime = ISOdatetime(.data$Year, .data$Month, .data$Day,
                             .data$Hour, .data$Minute, .data$Second),
      Hour = NULL,
      Minute = NULL,
      Second = NULL,
      Year = NULL,
      Month = NULL,
      Day = NULL) %>%
    dplyr::select(.data$Station, .data$Variable, .data$DateTime,
                  .data$Value, .data$Units, dplyr::everything())
}
