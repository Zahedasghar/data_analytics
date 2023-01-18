library(readr)
library(tidyverse)
#pakwheels_11Jul2020 <- read_csv("C:/Users/92300/Downloads/archive/pakwheels-11Jul2020.csv")
#pakwheels<-saveRDS(pakwheels_11Jul2020,file = "pakwheels.rds")

pakwheels <- readRDS("D:/RepTemplates/data_analytics/pakwheels.rds")

pakwheels|>glimpse()
pakwheels|>glimpse()|>na.omit()


pakwheels$Price<-as.numeric(pakwheels$Price)
View(pakwheels)
pakwheels|>distinct(`Model Year`)

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
