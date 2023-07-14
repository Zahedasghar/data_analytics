library(tidyverse)
library(lubridate)
sp500_opens <- gt::sp500 |> 
  select(date,open)

sp500_opens

#Manually with {lubridate}

sp500_opens |> 
  mutate(
    day=day(date),
    month=month(date),
    year=year(date)
  )

sp500_opens |> 
  timetk::tk_augment_timeseries_signature(date)


library(tidyverse)
library(lubridate)
sp500_opens <- gt::sp500 |> 
  select(date, open)
# # A tibble: 16,607 × 2
# date        open
# <date>     <dbl>
#   1 2015-12-31 2061.
# 2 2015-12-30 2077.
# 3 2015-12-29 2061.
# 4 2015-12-28 2058.
# 5 2015-12-24 2064.
# # ℹ 16,602 more rows

# Manually with {lubridate} :(
sp500_opens |> 
  mutate(
    day = day(date),
    month = month(date),
    year = year(date)
  )
# # A tibble: 16,607 × 5
# date        open   day month  year
# <date>     <dbl> <int> <dbl> <dbl>
#   1 2015-12-31 2061.    31    12  2015
# 2 2015-12-30 2077.    30    12  2015
# 3 2015-12-29 2061.    29    12  2015
# 4 2015-12-28 2058.    28    12  2015
# 5 2015-12-24 2064.    24    12  2015
# # ℹ 16,602 more rows

# {timetk} computes the same AND MORE
sp500_opens |> 
  timetk::tk_augment_timeseries_signature(date)
# A tibble: 16,607 × 30
# date          open  index.num   diff  year year.iso  half quarter month month.xts month.lbl
# <date>        <dbl>      <dbl>  <dbl> <int>    <int> <int>   <int> <int>     <int> <ord>    
# 1 1950-01-03  16.7 -630979200     NA  1950     1950     1       1     1         0 Januar   
# 2 1950-01-04  16.8 -630892800  86400  1950     1950     1       1     1         0 Januar   
# 3 1950-01-05  16.9 -630806400  86400  1950     1950     1       1     1         0 Januar   
# 4 1950-01-06  17.0 -630720000  86400  1950     1950     1       1     1         0 Januar   
# 5 1950-01-09  17.1 -630460800 259200  1950     1950     1       1     1         0 Januar   
# # ℹ 16,602 more rows
# # ℹ 18 more variables: hour <int>, minute <int>, second <int>, hour12 <int>, am.pm <int>,
# #   wday <int>, wday.xts <int>, wday.lbl <ord>, mday <int>, qday <int>, yday <int>,
# #   mweek <int>, week <int>, week.iso <int>, week2 <int>, week3 <int>, week4 <int>,
# #   mday7 <int>


## Create your own function that extracts only your desired time units
get_selected_time_signature <- function(data, date_var, time_units) {
  data |> 
    timetk::tk_augment_timeseries_signature({{date_var}}) |> 
    select(
      all_of(
        c(colnames(data), time_units)
      )
    )
}

sp500_opens |> 
  get_selected_time_signature(
    date_var = date,
    time_units = c('year', 'quarter', 'month', 'day')
  )
# # A tibble: 16,607 × 6
# date        open  year quarter month   day
# <date>     <dbl> <int>   <int> <int> <int>
#   1 1950-01-03  16.7  1950       1     1     3
# 2 1950-01-04  16.8  1950       1     1     4
# 3 1950-01-05  16.9  1950       1     1     5
# 4 1950-01-06  17.0  1950       1     1     6
# 5 1950-01-09  17.1  1950       1     1     9
# # ℹ 16,602 more rows