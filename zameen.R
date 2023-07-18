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
#'  How many total properties listed?
#'  which city has maximum properties listed?
#'  Whch city has maximum properties listed by area in square feel?
#'  Make a histogram of area, prices
#'  Make a boxplot of #number of bed rooms , #of bath rooms
#'  Creata variable price per square feet and store it as price_prsqft
#'  Make a histogram of this price_prsqft for rental and for sale properties
#'  Make a boxplot of price_prsqft across cities for Sale and for Rental properties.
#'  Make a scatter plot of price and area in square ft
#'  log of price per square ft and area
#'  boxplot of priceprsqft and bedrooms
#'  boxplot of price_prsqft and bathrooms
#'  
#'  
#'  GT Tables



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

#' Give a table of variables in nice format 


zameen |> glimpse()

# Which day of the week maximum properties are made available on zameen.com

# From glimpse we can observe nature of variables and can drop variables which are
# redundant for us for this analysis.
# so we drop property_id and location_id, latitude, longitude,
# locality (as both city and location are already availe)
# Similarly area which is character variable is given in Kamals, Marlas...
# while area_marla and area_sqft
# are more useful.


zameen |>select(-c(property_id,location_id,page_url,locality, area, latitude,longitude)) |>  clean_names() -> zam_sel
zam_sel |> glimpse()

zam_sel <- zam_sel |> mutate(date = mdy(date_added))

library(gt)
library(gtExtras)

zam_sel |> group_by(city) |> count() |> arrange(-n) |> gt()

zam_sel |> group_by(year) 


zam_sel |> mutate(price_m = price / 1000000) -> zam_sel


summary(zam_sel$area_sqft)


## What type of properties are listed
zam_sel |> select(property_type, purpose) |> tbl_summary()
## List properties by frequency
zam_sel |> group_by(property_type, purpose) |>
  summarise(property_types = n()) |> gt()




zam_sel |> filter(area_marla > 5 & area_marla < 12) |>  group_by(city) |>
  arrange(-price_m) |> select(price, city, price_m) |> top_n(n = 5) |> gt()


zam_sel |> select(purpose, price) |> tbl_summary() |> as_gt()


zam_sel |> select(purpose,price,year) |> tbl_summary()


## One variable : median

zam_sel |>filter(purpose=="For Rent", area_marla>1 & area_marla<40) |>  select(area_marla)  |> summarise(avg_size=mean(area_marla),med_size=median(area_marla),
                                          min_size=min(area_marla),max_size=max(area_marla))



zam_sel |>filter(purpose=="For Rent", area_marla>1 & area_marla<40) |> 
  ggplot(aes(area_marla)) +
  geom_histogram()


#' some improbable values are included
#' To avoid this apply filters 

zam_sel |> select(area_sqft)  |> summarise(avg_size=mean(area_sqft),med_size=median(area_sqft),
                                           min_size=min(area_sqft),max_size=max(area_sqft))


# Number of agencies city wise 

zam_sel |> group_by(agency) |> filter(city=="Islamabad"|city=="Rawalpindi") |> 
  summarise(n=n()) |> arrange(-n) |> na.omit() |> filter(n>50) 


# Top 10  agencies in Lahore
zam_sel |> group_by(agency) |> filter(city=="Lahore") |> 
  summarise(n=n()) |> arrange(-n) |> na.omit() |> filter(n>50) 


## Which place in Islamabad has maximum properties for rent

zam_sel |> group_by(location) |> filter(city=="Islamabad", purpose=="For Rent") |> 
  summarise(n=n()) |> arrange(-n) |> na.omit() |> top_n(5) 

## Which place in Islamabad has maximum properties for sale

zam_sel |> group_by(location) |> filter(city=="Islamabad", purpose=="For Sale") |> 
  summarise(n=n()) |> arrange(-n) |> na.omit() |> top_n(10) 

## Which place in Islamabad has maximum properties for sale

zam_sel |> group_by(location) |> filter(city=="Rawalpindi", purpose=="For Sale") |> 
  summarise(n=n()) |> arrange(-n) |> na.omit() |> top_n(10) 

## Which size properties are more in number


## Histogram of a price variable

ggplot(zam_sel)+aes(price)+geom_histogram()


ggplot(zam_sel)+aes(log(price))+geom_histogram()

zam_sel |> filter(area_marla==5) |> 
  ggplot()+aes(log(price))+geom_histogram()

## Why two peaks, because one is rental, other one is for sale
zam_sel |> filter(area_marla==5, purpose=="For Rent") |> arrange(-price) |> top_n(5)
zam_sel |> filter(area_marla==5, purpose=="For Rent", price<1.30e5) |> 
  ggplot()+aes(price)+geom_histogram(bins=10)

zam_sel |> filter(area_marla==5, purpose=="For Rent", price<1.30e5) |> 
  ggplot()+aes(log(price), fill="grey")+geom_histogram(bins=10, col="blue")+theme_minimal()


zam_sel |> filter(area_marla==5, purpose=="For Sale", price>3e6) |> 
  ggplot()+aes(log(price), fill="grey")+geom_histogram(bins=15, col="blue")+theme_minimal()


## Compare prices of 5 Marla across cities

zam_sel |>filter(purpose=="For Rent", area_marla==5) |>  
  ggplot(aes(x=city, y=price))+
  geom_boxplot()


zam_sel |>filter(purpose=="For Rent", area_marla==5) |>  
  ggplot(aes(x=city, y=log(price)))+
  geom_boxplot()


zam_sel |>filter(purpose=="For Sale", area_marla==5) |>  
  ggplot(aes(x=city, y=log(price)))+
  geom_boxplot()




zam_sel |> filter(city=="Islamabad", area_sqft<8000, purpose=="For Rent", price>15000) |> 
ggplot()+
  aes(x=area_sqft,y=log(price))+ geom_point() +
  geom_smooth()
