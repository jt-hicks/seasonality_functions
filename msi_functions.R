##Calculate MSI and angle/date of seasonal peak
return_msi <- function(dataframe,measure,single_year=FALSE){
  dataframe$year <- lubridate::year(dataframe$date)
  dataframe$month <- lubridate::month(dataframe$date)
  dataframe$value <- dataframe[,measure,drop=TRUE]
  ##Create key to convert radians/degrees to date
  month_arc <- data.frame(date = seq.Date(from=as.Date('2010-01-01'),to=as.Date('2010-12-31'),by='days'),
                          degree = 0:364)%>%
    mutate(radians = degree*pi/180,
           mid_month = as.character(zoo::as.Date(zoo::as.yearmon(date),frac=0.5)),##Set date to middle of month
           month=lubridate::month(date))%>%
    filter(date==as.Date(mid_month))%>%
    select(month,degree,radians)

  if(single_year){
    if(nrow(dataframe)!=12){
      stop('There need to be 12 months of data to calculate MSI for a single year.')
    }
    dataframe$year = 1
  }
  ##Calculate MSI
  output <- dataframe %>%
    left_join(month_arc, by=join_by(month))%>%
    mutate(sin = value*sin(radians),
           cos = value*cos(radians))%>%
    group_by(year)%>% #if more than one year, calculate MSI for each year
    summarise(rk = sqrt(sum(sin)^2 + sum(cos)^2), #Total vector length
              theta_k = atan2(sum(sin),sum(cos)), #Peak angle in radians
              theta_k_deg = (theta_k*180/pi + 360) %% 360, #Peak angle in degrees
              msi = rk/sum(value),
              month_num=n()) %>%
    filter(month_num==12) %>% ##Remove year if less than 12 months given
    select(-month_num)

  return(output)
}

##Return date from angle (in degrees)
date_from_angle <- function(angle,year=2010){
  return(as.Date(date_decimal(year+angle/360)))
}
