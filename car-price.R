library(readxl)
library(tidyverse)
library(forcats)
car_prices <- read_excel("car-prices.xlsx")
# Remove missing values %>% 
car_prices<- car_prices |> na.omit()
View(car_prices)
#remove dollar signs from sales column
#https://uc-r.github.io/lollipop
#https://www.statology.org/remove-dollar-sign-in-r/

car_prices|>glimpse()
car_prices$Prices = as.numeric(gsub("[\\PKR,]", "", car_prices$Prices))
car_prices|>glimpse()

## To get first letter as Make
car_prices$Make <- gsub("([A-Za-z]+).*", "\\1", car_prices$Cars)
car_prices|>glimpse()
carp<-car_prices|>mutate(prices=Prices/1000000)
carp|>filter(Make=="Suzuki")|> arrange(prices)|>
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

library(gt)
library(gtExtras)
carp|>filter(Make=="Honda")|> mutate(Cars = fct_reorder(Cars, prices))|>
  gt()|>gt_theme_guardian()|>tab_header("Honda vehicles prices in Jan-2023")
  

