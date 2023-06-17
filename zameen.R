library(readr)
library(tidyverse)
library(janitor)
library(lubridate)
#zameen <- read_csv("data/Property.csv")


#saveRDS(zameen, "zameen.rds")
#zameen <- readRDS("zameen.rds")
zameen <- readRDS("D:/RepTemplates/data_analytics/zameen.rds")

zameen |> glimpse()

zameen |> clean_names()->zameen

zameen$date <- mdy(zameen$date_added)

zameen |> glimpse()

zameen |> group_by(city) |> count() |> arrange(-n)

zameen |> group_by(year) 


zameen |> mutate(price_m=price/1000000) -> zameen 




zameen |> filter(area_marla>5 & area_marla<12) |>  group_by(city) |> arrange(-price_m) |>select(price, city, price_m) |> top_n(n=5) 




