library(readr)
library(tidyverse)
library(gt)
library(gtExtras)
#pakwheels_11Jul2020 <- read_csv("C:/Users/92300/Downloads/archive/pakwheels-11Jul2020.csv")
#pakwheels<-saveRDS(pakwheels_11Jul2020,file = "pakwheels.rds")

pakwheels <- readRDS("D:/RepTemplates/data_analytics/pakwheels.rds")

pakwheels|>glimpse()
pakwheels|>glimpse()|>na.omit()


pakwheels$Price<-as.numeric(pakwheels$Price)
View(pakwheels)
pakwheels|>distinct(`Model Year`)|>arrange(`Model Year`)

## Select
pakwheels<-pakwheels|>select(-URL)

pakwheels|>count(Color)|>arrange(desc(n))

pakwheels$Make <- gsub("([A-Za-z]+).*", "\\1", pakwheels$Name)

pakwheels|>count(Make)|>arrange(desc(n))
## Honda colors
pakwheels|>filter(Make=="Toyota")|>count(Color)|>arrange(desc(n))

pakwheels|>filter(Make=="Suzuki")|>count(Color)|>arrange(desc(n))

pakwheels$hp <- gsub("([A-Za-z]+).*", "", pakwheels$`Engine Capacity`)

pakwheels$hp<-as.numeric(pakwheels$hp)
pakwheels|>glimpse()

pakwheels|>group_by(Make)|>count(hp)|>arrange(desc(n))

pakwheels|>select(hp)|>arrange(desc(hp))
pakwheels|>filter(Make=="Honda")|>count(hp)|>arrange(desc(n))|>gt()

pakwheels|>filter(hp>=600 & hp<=3000)|>
ggplot(aes(x=hp,y=Price))+geom_point()


pakwheels|>filter(hp>=600 & hp<=3000,`Model Year`==2009)|>
  ggplot(aes(x=hp,y=Price,color=Make))+geom_point()


pakwheels|>filter(hp>=600 & hp<=2000,`Model Year`==2019)|>
  ggplot(aes(x=hp,y=Price,color=Assembly))+geom_point()+geom_smooth(method = "lm")



pakwheels|>filter(hp>=600 & hp<=2000,`Model Year`==2017)|>
ggplot(aes(x=Mileage,y=Price))+geom_point()

pakwheels|>filter(hp>=600 & hp<=2000,`Model Year`==2017, Price<6000000)|>
  ggplot(aes(x=Price,color=Make))+geom_boxplot()+coord_flip()


library(gt)
library(gtExtras)
pakwheels |> filter(hp==1300) |>select(Price,Make)|> gt_plt_summary()

pakwheels |> filter(hp==1300,`Model Year`==2019)|> group_by(Make)|>na.omit()|>
  summarise(avg=mean(Price)) |>
  arrange(desc(avg))|>
  gt()

pakwheels |> filter(hp==1500,`Model Year`==2019)|> group_by(Make)|>na.omit()|>
  summarise(avg=mean(Price)) |>
  arrange(desc(avg))|>
  gt()

pakwheels |> filter(hp==1800,`Model Year`==2019)|> group_by(Make)|>na.omit()|>
  summarise(avg=mean(Price)) |>
  arrange(desc(avg))|>
  gt()

pakwheels|>filter(`Model Year`==2019)|>
  ggplot(aes(x=hp,y=Price))+geom_point()+
  geom_smooth(method = "lm")


pakwheels|>filter(hp<2000,`Model Year`==2019)|>
  ggplot(aes(x=hp,y=log(Price)))+geom_point()+
  geom_smooth(method = "lm")

pakwheels|>filter(hp<2000&hp>500,`Model Year`==2019)|>
  ggplot(aes(x=hp,y=log(Price)))+geom_point()+
  geom_smooth(method = "loess")

pakwheels|>filter(hp<2000&hp>500,`Model Year`==2019)|>
  ggplot(aes(x=hp,y=log(Price)))+geom_point()+
  geom_smooth(method = "lm")

library(modelsummary)
modelsummary(lm(log(Price)~hp,data=pakwheels))


manufacturers <- pakwheels |> 
  count(Make, sort = TRUE) |> 
  mutate(
    manufacturer = str_to_title(Make),
    manufacturer = fct_reorder(Make, n) 
  )|>na.omit()

manufacturers

## Bar plot
manufacturers |> filter(n>200)|>
  ggplot(aes(y = manufacturer, x = n)) +
  geom_col(fill = 'dodgerblue4') +
  theme_minimal() +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = element_blank(), 
    y = element_blank(),
    title = 'Number of vehicles in the Pakwheels data set',
    subtitle = "At least 200 vehicles should be in the data to be included in graph",
    caption = "Source: Pakwheels| Zahid Asghar "
  ) +
  theme(
    panel.grid.major.y = element_blank()
  )

## Lollipop chart
manufacturers |> filter(n>200)|>
  ggplot(aes(y = manufacturer, x = n)) +
  geom_point(col = 'dodgerblue4', size = 5) +
  geom_segment(
    aes(x = 0, xend = n, y = manufacturer, yend = manufacturer),
    linewidth = 1.5,
    col = 'dodgerblue4'
  ) +
  theme_minimal() +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = element_blank(), 
    y = element_blank(),
    title = 'Number of vehicles in the Pakwheels data set',
    subtitle = "At least 200 vehicles should be in the data to be included in graph",
    caption = "Source: Pakwheels| Zahid Asghar "
  ) +
  theme(
    panel.grid.major.y = element_blank()
  )

pakwheels$price<-as.numeric(pakwheels$Price) ## To convert price as numeric, R-base command. There are other ways to do the same
pakwheels$hp <- as.numeric(gsub("([A-Za-z]+).*", "", pakwheels$`Engine Capacity`)) ## To take numeric values from variable Engine Capacity and lets give it a new name: hp.
pakwheels$company <- gsub("([A-Za-z]+).*", "\\1", pakwheels$Name)  ## To take first word from column Name for taking it a 
pakwheels<-pakwheels|>rename(year=`Model Year`)
pkw<- pakwheels|>na.omit()
pkw<- pkw %>% mutate(price_m=price/1000000)
pkw<- pakwheels|>na.omit()
pkw|>glimpse()
pkw|>filter(hp==1300)|>
  ggplot(aes(price,color=company,fill=company))+geom_histogram()
library(ggthemes)
pkw|>filter(hp==1300, year>2015, Assembly=="Local",price>600000)|>
  ggplot(aes(price_m,color=company,fill=company))+geom_density()+scale_x_continuous()+
  labs(x="Price in million of Rs",title = "Density plot for prices for 1300 cc autos",
       subtitle="Toyota cars have highest variation",
       caption="source:pakwheels,By Zahid Asghar")



pkw|>filter(hp==1300, year>2015, Assembly=="Local",price>600000, Mileage<250000)|>
  ggplot(aes(x=Mileage,y=price_m,color=company))+geom_point()+scale_x_continuous()+
  geom_smooth(method = "lm")+guides(colour="none")+
  labs(x="Mileage in km",title = "Relationship between price and mileage",
       
       caption="source:pakwheels,By Zahid Asghar")



pkw|>filter(hp==1300, year>2015, Assembly=="Local",price>600000, Mileage<100000)|>
  ggplot(aes(x=Mileage,y=price_m,color=company))+geom_point(size = 1.2,
                                                            alpha = 0.7,
                                                            shape = 16,
                                                            show.legend = FALSE)+scale_x_continuous()+
  geom_smooth(method = "lm")+scale_color_manual(values = c("cadetblue", "orchid", 
                                                                      "tomato","navy"))+
                                                                        labs(
    title = "Mileage and prices relationship",
    subtitle = "1300 cc 
    <span style = 'color:tomato'>**Suzuki**</span>,
    <span style = 'color:orchid;'>**Honda**</span>,
    <span style = 'color:cadetblue;'>**FAW**</span> and 
    <span style = 'color:navy;'>**Toyota**</span> cars",
   color="company") +labs(caption = "Source: pakwheels, By Zahid Asghar") +
  theme(plot.caption = element_text(size=16, color="red", face="italic"))+
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.subtitle = element_markdown())+guides(color="none")


pkw |> filter(hp==1300, price>1000000) |>
  group_by(year)|>
  summarise(avg=mean(price))





set.seed(123)
pkw|>filter(hp==1300, year>2015, Assembly=="Local",price>600000, Mileage<250000)|>
  ggplot(aes(x = Mileage, y = price_m)) +
  geom_jitter(
    size = 3,
    alpha = 0.7,
    shape = 16,
    width = 0.2,
    color = "cadetblue") +
  geom_vline(
    xintercept = seq(2000, length(unique(pkw$Mileage)), by = 10),
    color = "gray90",
    size = 1) +
  stat_summary(
    fun = "mean",
    geom = "crossbar",
    width = 0.5,
    color = "tomato",
    linetype = 2) +
  labs(
    title = "the effectiveness of various feed supplements",
    subtitle = "on the growth rate of chickens after six weeks",
    x = "feed type",
    y = "weight (grams)") +
  theme_minimal() +
  theme(panel.grid = element_blank())



require(ggplot2)

set.seed(123)

ggplot(chickwts, aes(x = feed, y = weight)) +
  geom_jitter(
    size = 3,
    alpha = 0.7,
    shape = 16,
    width = 0.2,
    color = "cadetblue") +
  geom_vline(
    xintercept = seq(1.5, length(unique(chickwts$feed)), by = 1),
    color = "gray90",
    size = 1) +
  stat_summary(
    fun = "mean",
    geom = "crossbar",
    width = 0.5,
    color = "tomato",
    linetype = 2) +
  labs(
    title = "the effectiveness of various feed supplements",
    subtitle = "on the growth rate of chickens after six weeks",
    x = "feed type",
    y = "weight (grams)") +
  theme_minimal() +
  theme(panel.grid = element_blank())
