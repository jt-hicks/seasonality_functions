##Calculate maximum proportion of a measure in a given month window,
##Averaged over length of the time series
highest_proportion <- function(data,measure,window){
  data$value <- data[,measure,drop=TRUE]
  data$year <- lubridate::year(data$date)
  data$month <- lubridate::month(data$date)
  df <- data %>%
    group_by(month)%>%
    summarise(month_av = mean(value))
  window_end <- window-1

  sums <- sapply(1:12, function(m){
    # Compute months in the window (wrap around using modulo arithmetic)
    months_in_window <- sapply(0:window_end, function(k) ((m + k - 1) %% 12) + 1)

    # Subset the data frame for these months
    subset_df <- df[df$month %in% months_in_window, ]

    # Sum the values
    return(sum(subset_df$month_av))
  })
  # Find the starting month with the maximum total value
  proportion <- sums/sum(df$month_av)
  return(data.frame(max_proportion = max(proportion),
                    month_start = which.max(proportion)))
}
##Calculate maximum proportion of a measure in a given month window
##Calculate for each year in dataset
highest_proportion_annual <- function(data,measure,window){
  data$value <- data[,measure,drop=TRUE]
  data$year <- lubridate::year(data$date)
  data$month <- lubridate::month(data$date)
  data$season <- ifelse(data$month<7,data$year,data$year+1)
  df <- data %>%
    group_by(season,month)
  window_end <- window-1

  yearly_proportions <- bind_rows(lapply((min(data$season)+1):(max(data$season)-1),function(y){
    df_year <- df[df$season==y,]
    sums <- sapply(1:12, function(m){
      # Compute months in the window (wrap around using modulo arithmetic)
      months_in_window <- sapply(0:window_end, function(k) ((m + k - 1) %% 12) + 1)

      # Subset the data frame for these months
      subset_df <- df_year[df_year$month %in% months_in_window, ]

      # Sum the values
      return(sum(subset_df$value))
    })
    proportion <- sums/sum(df_year$value)
    return(data.frame(max_proportion = max(proportion),
                      month_start = which.max(proportion),
                      year = y))

  }))
  # Find the starting month with the maximum total value
  return(yearly_proportions)
}
