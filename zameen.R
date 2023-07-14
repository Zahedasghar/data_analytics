#' This data are scrapped from zameen.com and courtesy to Ms.Fatima for this kind favour.
#' I am always obliged to Dr, M. Yaseen who always helped me and supported me in difficult
#' times while learning R and whenever I am stuck. His small tips helped me a lot over the last 3 years. 
#' I am using this data consisting of 191,393 properties listed on zameen.com.
#' Purpose of this exercise is to have learning for `Applied Data Science`.
#' This is not an analysis for any policy advice on investment 
#' Purpose of this post is
#' `Exploratory Data Analysis`
#' `How many variables in data set and describe nature of each variable`
#' `Model Fitting`
#' `Learning R for Applied Data Science tools`
#' 
#' `How to create a date variable`
#' `Convert prices into millions of Rs`
#' `How many properties are for rent, sale or other purpose`
#' `Which city has highest price for 10 Marla house for sale`
#' 
#' `Which city has highest price for 10 Marla house for rent`
#'  `How many years data are available `
#'  `What is maximum price for house for sale and in which city`
#'  `Which locality in Lahore has highest rent `
#'  `List top 10 agents`
#'  
#'  
#'  ``




library(readr)
library(tidyverse)
library(janitor)
library(lubridate)
# zameen <- read_csv("data/Property.csv")
# 
# save(zameen, file = "data/zameen.RData")
load("data/zameen.RData")
#saveRDS(zameen, "zameen.rds")
#zameen <- readRDS("zameen.rds")
# zameen <- readRDS("D:/RepTemplates/data_analytics/zameen.rds")

tbl <- as_tibble(zameen)
variable_info <-
  tbl |> summarise_all(class) |> pivot_longer(everything(), names_to = "Variable", values_to = "Class")

variable_info |> gt()




zameen |> glimpse()

zameen |> clean_names() -> zameen

zameen <- zameen |> mutate(date = mdy(date_added))

library(gt)
library(gtExtras)

zameen |> group_by(city) |> count() |> arrange(-n) |> gt()

zameen |> group_by(year) 


zameen |> mutate(price_m = price / 1000000) -> zameen


summary(zameen$area_sqft)


## What type of properties are listed
zameen |> select(property_type, purpose) |> tbl_summary()
## List properties by frequency
zameen |> group_by(property_type, purpose) |>
  summarise(property_types = n()) |> gt()




zameen |> filter(area_marla > 5 & area_marla < 12) |>  group_by(city) |>
  arrange(-price_m) |> select(price, city, price_m) |> top_n(n = 5) |> gt()


zameen |> select(purpose, price) |> tbl_summary() |> as_gt()


zameen |> select(purpose,price,year) |> tbl_summary()


# Number of agencies city wise 

zameen |> group_by(agency) |> filter(city=="Islamabad"|city=="Rawalpindi") |> 
  summarise(n=n()) |> arrange(-n) |> na.omit() |> filter(n>50) 


# Top 10  agencies in Lahore
zameen |> group_by(agency) |> filter(city=="Lahore") |> 
  summarise(n=n()) |> arrange(-n) |> na.omit() |> filter(n>50) 


## Which place in Islamabad has maximum properties for rent

zameen |> group_by(location) |> filter(city=="Islamabad", purpose=="For Rent") |> 
  summarise(n=n()) |> arrange(-n) |> na.omit() |> top_n(5) 

## Which place in Islamabad has maximum properties for sale

zameen |> group_by(location) |> filter(city=="Islamabad", purpose=="For Sale") |> 
  summarise(n=n()) |> arrange(-n) |> na.omit() |> top_n(10) 

## Which place in Islamabad has maximum properties for sale

zameen |> group_by(location) |> filter(city=="Rawalpindi", purpose=="For Sale") |> 
  summarise(n=n()) |> arrange(-n) |> na.omit() |> top_n(10) 

## Which size properties are more in number


## Histogram of a price variable

ggplot(zameen)+aes(price)+geom_histogram()


ggplot(zameen)+aes(log(price))+geom_histogram()

zameen |> filter(area_marla==5) |> 
  ggplot()+aes(log(price))+geom_histogram()

## Why two peaks, because one is rental, other one is for sale
zameen |> filter(area_marla==5, purpose=="For Rent") |> arrange(-price) |> top_n(5)
zameen |> filter(area_marla==5, purpose=="For Rent", price<1.30e5) |> 
  ggplot()+aes(price)+geom_histogram(bins=10)

zameen |> filter(area_marla==5, purpose=="For Rent", price<1.30e5) |> 
  ggplot()+aes(log(price), fill="grey")+geom_histogram(bins=10, col="blue")+theme_minimal()


zameen |> filter(area_marla==5, purpose=="For Sale", price>3e6) |> 
  ggplot()+aes(log(price), fill="grey")+geom_histogram(bins=15, col="blue")+theme_minimal()


## Compare prices of 5 Marla across cities

zameen |>filter(purpose=="For Rent", area_marla==5) |>  
  ggplot(aes(x=city, y=price))+
  geom_boxplot()


zameen |>filter(purpose=="For Rent", area_marla==5) |>  
  ggplot(aes(x=city, y=log(price)))+
  geom_boxplot()


zameen |>filter(purpose=="For Sale", area_marla==5) |>  
  ggplot(aes(x=city, y=log(price)))+
  geom_boxplot()




zameen |> filter(city=="Islamabad", area_sqft<8000, purpose=="For Rent", price>15000) |> 
ggplot()+
  aes(x=area_sqft,y=log(price))+ geom_point() +
  geom_smooth()
