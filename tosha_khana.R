library(readr)
library(tidyverse)
library(readxl)
tosha <- read_csv("data/tosha_khana1.csv")
library(lubridate)
library(janitor)
tosha |> clean_names() -> tosha
tosha$date <- mdy(tosha$date)
tosha
tail(tosha)
tosha <- tosha |> select(-c(x9,x10,x11)) |> slice(1:4432)
tosha |> mutate(month = lubridate::month(date, label = FALSE),
                year = lubridate::year(date)) |> arrange(year) ->tkhana

#tkhana |> mutate(value=parse_number(assessed_value)) |> View()
tkhana$value<- as.numeric(tkhana$value)
#View(tkhana)
colnames(tkhana)
tkhana1 <- cSplit(tkhana,"name_of_recipient",sep=",")

tkhana1

#install.packages("splitstackshape")

library(splitstackshape)

tkhana1 |> filter(name_of_recipient_2=="Prime Minister of Pakistan")  |> filter(year>2017&year<2022) |> 
  View()






tk <- read_csv("data/tosha.csv")
tk
library(lubridate)
library(janitor)
tk|> clean_names() -> tk
tk$date <- mdy(tk$date)
tk
tail(tosha)
tosha <- tosha |> select(-c(x9,x10,x11)) |> slice(1:4432)
tosha |> mutate(month = lubridate::month(date, label = FALSE),
                year = lubridate::year(date)) |> arrange(year) ->tkhana

#tkhana |> mutate(value=parse_number(assessed_value)) |> View()
tkhana$value<- as.numeric(tkhana$value)
#View(tkhana)
colnames(tkhana)
tkhana1 <- cSplit(tkhana,"name_of_recipient",sep=",")
