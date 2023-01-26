library(readxl)
library(readr)
library(tidyverse)
library(forcats)
car_prices <- read_excel("car-prices.xlsx")
View(car_prices)
# Remove missing values as t 
car_prices<- car_prices |> na.omit()

#remove dollar signs from sales column
#https://uc-r.github.io/lollipop
#https://www.statology.org/remove-dollar-sign-in-r/
 
# Lets have an overview of car price data
car_prices |> glimpse()

# Price is a character variable and we want it to be numeric variable
car_prices$Prices<-  parse_number(car_prices$Prices)
car_prices|>glimpse() ## Now Prices is a double (numeric variable) and it has PKR written before it
## This is another way of doing the same thing
#car_prices$Prices = as.numeric(gsub("[\\PKR,]", "", car_prices$Prices))


## To get first letter as Make

car_prices$Make <- gsub("([A-Za-z]+).*", "\\1", car_prices$Cars)
car_prices|>glimpse()
## To convert price into millions of Rs.
carp<-car_prices|>mutate(prices=Prices/1000000)
carp|>filter(Make=="Suzuki")|> mutate(Cars = fct_reorder(Cars, prices))|>
  ggplot(aes(x=Cars,y=prices))+geom_bar(stat = "identity")+coord_flip()

carp|>filter(Make=="Honda")|> mutate(Cars = fct_reorder(Cars, prices))|>
  ggplot(aes(x=Cars,y=prices))+geom_col()+coord_flip()


carp|>filter(Make=="Honda")|> mutate(Cars = fct_reorder(Cars, prices))|>
  ggplot(aes(y=Cars,x=prices,label=round(prices,2)))+
geom_segment(aes(x = 0, y =Cars, xend = prices, yend = Cars), color = "blue") +
  geom_point(size = 10) +
  geom_text(color = "white", size = 3)+
    labs(title = "Toyota vehicles pricess Jan-2023 in million of Rs.",
       caption = "Zahid Asghar") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        text = element_text(family = "Georgia"),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 20, margin = margin(b = 10), hjust = 0),
        plot.subtitle = element_text(size = 12, color = "darkslategrey", margin = margin(b = 25, l = -25)),
        plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey70", hjust = 0))

## How to create beautifull tables

library(gt)
library(gtExtras)
carp|>filter(Make=="Honda")|> select(Cars,prices)|>  mutate(Cars = fct_reorder(Cars, prices))|>
  gt()|>gt_theme_pff()|>tab_header("Honda vehicles prices(mill. of PKR) in Jan-2023")
  

## Same table with New York times theme

carp|>filter(Make=="Honda")|> select(Cars,prices)|>  mutate(Cars = fct_reorder(Cars, prices))|>
  gt()|>gt_theme_nytimes()|>tab_header("Honda vehicles prices(mill. of PKR) in Jan-2023")


carp|>filter(Make=="Honda")|> select(Cars,prices)|>  mutate(Cars = fct_reorder(Cars, prices))|>
  gt()|>gt_theme_pff()|> 
  tab_source_note("Business Recorder")|>tab_caption("Honda vehicels prices (mill. of PKR) Jan 2023")
