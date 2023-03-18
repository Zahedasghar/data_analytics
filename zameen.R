library(readr)
library(tidyverse)
library(janitor)
library(lubridate)
#zameen <- read_csv("data/Property.csv")


#saveRDS(zameen, "zameen.rds")
readRDS("zameen.rds")


zameen |> glimpse()

zameen |> clean_names()->zameen

zameen$date <- mdy(zameen$date_added)

zameen |> glimpse()

zameen |> group_by(city) |> count() |> arrange(-n)

zameen |> group_by(year) |> count()
