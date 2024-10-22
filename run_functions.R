source('msi_functions.R')
source('proportion_functions.R')
library(dplyr)
library(ggplot2)

##Run functions

##Example data
example_data <- data.frame(date = seq.Date(from=as.Date('2010-01-01'),to=as.Date('2010-12-01'),by='month'),
                           cases = c(10,12,14,18,24,38,35,22,20,10,8,2),
                           prevalence = c(0.5,0.49,0.44,0.45,0.48,0.5,0.52,0.55,0.56,0.54,0.53,0.52))
example_data_manyyears <- bind_rows(lapply(0:5, function(x){
  if(x==0){
    return(example_data)
  } else {
    new_year <- example_data %>%
      mutate(date = lubridate::ymd(date) + lubridate::years(x),
             cases = cases + rpois(12,lambda=x),
             prevalence = rnorm(12,prevalence,sd=0.03))
  }

}))

##MSI
cases_msi <- return_msi(dataframe=example_data,
           measure='cases',
           single_year = TRUE)
prevalence_msi <- return_msi(dataframe=example_data,
                        measure='prevalence',
                        single_year = TRUE)
##MSI multiple year
cases_msi_multi <- return_msi(dataframe=example_data_manyyears,
                        measure='cases',
                        single_year = FALSE)
prevalence_msi_multi <- return_msi(dataframe=example_data_manyyears,
                             measure='prevalence',
                             single_year = FALSE)

##Proportion in year
cases_prop <- highest_proportion(data=example_data,
                                 measure = 'cases',
                                 window=4)
prevalence_prop <- highest_proportion(data=example_data,
                                 measure = 'prevalence',
                                 window=4)

##Proportion in multiple years
cases_prop_multi <- highest_proportion_annual(data=example_data_manyyears,
                                 measure = 'cases',
                                 window=4)
prevalence_prop_multi <- highest_proportion_annual(data=example_data_manyyears,
                                      measure = 'prevalence',
                                      window=4)
