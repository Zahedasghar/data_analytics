## Always start with a project
## getwd()
## setwd()


## Lesson Plan

## Reading Data
## Using Required package
## data frame
## glimpse (to have an overview of data, rows, variables, variables nature)
## structure of data with `str` command , similar as glimpse from base R
## head
## tail
## View



## Select 
## Summarise  (mean, min, max, median, sd,q1, q3)
## filter
## arrange
## mutate
## rename (dont use names with gap  or starting with letters)
## 10 most powerful tools






library(tidyverse)
library(readr)  # To read csv file 

alif1 <- read_csv("Alifailan.csv", col_names = TRUE)


alif <- as_tibble(alif1)
alif
alif|>glimpse()
str(alif)
head(alif)
tail(alif)
View(alif)


## Select only few variables

alif |> select(Rank.2016,District, Province, infrastructure.score, Electricity,
               Drinking.water)

## Select data for 4 main provinces (two ways either filter or select)
alif %>% filter(Province!="AJK",Province!="GB",Province!="ICT",Province!="FATA")

## Or just dont select few variables

alif |> select(-Toilet,-Boundary.wall, -Building.condition.satisfactory)


## Slice
alif |> slice(1:5, )

alif |> select(District, Province, infrastructure.score) |> slice(25:30)
## Count total number of districts in each province

alif |> summarise(count = n(), .by = Province)

## Arrange ascending or descending order

alif |> summarise(count=n(),.by=Province)|>arrange(count)

## Descending

alif |> summarise(count = n(), .by = Province) |> arrange(desc(count))

## Filter 
alif |> summarise(count = n(), .by = Province) |> arrange(desc(count)) |>
  filter(count > 10)


alif |> summarise(count = n(), .by = Province) |> arrange(desc(count)) |>
  filter(count > 10) |>
  na.omit()

## Summarise continued

alif |> summarise(avg_iscr=mean(infrastructure.score),min=min(infrastructure.score),
                  max=max(infrastructure.score),median=median(infrastructure.score),
                  sd=sd(infrastructure.score),
                  .by = Province)




## Summarisaton for percentiles
alif %>%
  group_by(Province) %>%
  summarise(quants = list(quantile(Electricity, probs = c(.01, .1, .25, .5, .75, .90,.99)))) %>%
  unnest_wider(quants)


















## Lets build scatter plot between drinking water and toilet facilty

p<-ggplot(alif,aes(x=Drinking.water,y=Toilet,color=Province))+geom_point() ## Name plot as p
p
## Make comment on plot by observing where does each province districts lie
p+facet_wrap(~Province,ncol = 4)+
  labs(x="Drinking Water",y="Toilet Facility",title = "Toilet facility and Drinking Water Facility situation province wise",
       caption = "Alif Ailan Data 2016")

