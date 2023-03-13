
### install.packages("dplyr")

library(tidyverse)
library(svglite)

## .dta , .sav, .xlsx, .csv

library(readr)  # To read csv file

## alif1<-read.csv("docs/Alifailan.csv",header = TRUE) 


alif <- read_csv("docs/Alifailan.csv", col_names = TRUE)

  names(alif)

## Modify column names 
library(janitor)

alif |> clean_names() -> alif


## select()

## filter()


## arrange()

## mutate()

## summarise()







alif |> glimpse()  


glimpse(alif)

str(alif)

head(alif)

tail(alif)

View(alif)

names(alif)


## Rename
 
library(janitor)
alif<-alif |> clean_names() |> glimpse()
  

# alif <-   alif |> rename(inf_scr = infrastructure.score,
 #   drink_w = Drinking.water,    bound_w = Boundary.wall,
#    build_cond = Building.condition.satisfactory  )


## Select data for 4 main provinces (two ways either filter or select)
alif |> filter(province != "AJK",
                  province != "GB",
                  province != "ICT",
                  province != "FATA")
## Or use filter as

alif |> filter (province %in% c("Punjab","Sind","Balochistan", "KP"))

 
## How many districts in each province

alif |>  count(province, name="District.count")

alif |> count(province, name="dist_count") |> 
  arrange(dist_count)

library(gt)
library(gtsummary)
library(gtExtras)

alif |> gt_plt_summary()

alif |> select_if(is.numeric) |>  gt_plt_summary()


## If Punjab

alif |> filter(province=="Punjab") |> select_if (is.numeric) |> select(-rank_2016) |> 
  gt_plt_summary() |> gt_theme_guardian() |> tab_header(title = "School conditions in Punjab according to Alif Ailan Data")

alif |> count(province, name="dist_count") |> 
  arrange(desc(dist_count)) |> gt() |>
  gt_theme_pff() 

alif |> select(province, infrastructure_score) |> 
  tbl_summary(by=province)
 
colnames(alif) 

alif |> select(district, province, drinking_water)

alif |> select(-district, -rank_2016)

alif |>  count(province, name="district_count")|>
  arrange( desc(district_count)) |> as.data.frame()
 
## Modify column names 
library(janitor)
library(flipbookr)

alif |> clean_names() -> alif


## plots

library(ggplot2)

alif |> filter(province==Punjab) ## why an error

alif |> filter(province=="Punjab")

alif |> filter(province=="Punjab") |> 
  select(-rank_2016,-province) |> 
  slice(5:8)
  

ggplot(alif) 

ggplot(alif)+aes(x=drinking_water,y=electricity) 

ggplot(alif)+aes(x=drinking_water,y=electricity) +
  geom_point()
  

ggplot(alif)+aes(x=drinking_water,y=electricity) +
  geom_point() +
  aes(color=province)


ggplot(alif)+aes(x=drinking_water,y=electricity) +
  geom_point() +
  aes(color=province)+
  facet_wrap(~province)


ggplot(alif) + aes(x = drinking_water, y = electricity) +
  geom_jitter(width = .25,
              height = .25) +
  aes(col = province)+
  labs(title="Relationship between drinking water and electricity among districts")

ggplot(alif) + aes(x = drinking_water, y = electricity) +
  geom_jitter(width = .25,
              height = .25) +
  aes(col = province)+scale_color_discrete(guide = F)+
  labs(title="Relationship between drinking water and electricity among districts")+
  labs(caption = "Data source: Alif Ailan")




## Only for Punjab

alif |> filter(province=="Punjab")

alif |> filter(province=="Punjab") |> 
  ggplot()+aes(x=drinking_water,y=electricity)




alif |> filter(province=="Punjab") |> 
  ggplot()+aes(x=drinking_water,y=electricity)+
  geom_point() 
## Concept of ecological correlations

alif |> filter(province=="Punjab") |> 
  ggplot()+aes(x=drinking_water,y=electricity)+
  geom_point()



















 ## Lets build scatter plot between drinking water and toilet facilty

p<-ggplot(alif,aes(x=drinking_water,y=toilet,color=province))+geom_point() ## Name plot as p
p
## Make comment on plot by observing where does each province districts lie
p+facet_wrap(~province,ncol = 4)+
  labs(x="Drinking Water",y="Toilet Facility",title = "Toilet facility and Drinking Water Facility situation province wise",
       caption = "Alif Ailan Data 2016")

alif |> glimpse()
ggplot(alif)+aes(x=province,y=drinking_water, fill=province)+geom_boxplot(method="restyle")


alif |> filter(province=="Punjab") |> 
ggplot()+aes(x=reorder(district,toilet),y=toilet)+geom_bar(stat="identity")+
  coord_flip()+geom_col(fill = 'dodgerblue4') +
  theme_minimal() 
alif$drinking_water

alif |> filter(province=="Balochistan") |> 
  ggplot()+aes(x=reorder(district,drinking_water),y=drinking_water)+geom_bar(stat="identity")+
  coord_flip()+geom_col(fill = 'dodgerblue4') +
  theme_minimal() 


alif |> filter(province=="Balochistan") |> 
  ggplot()+aes(x=reorder(district,drinking_water),y=drinking_water)+geom_point(size=2,colour="orange")+
  geom_segment(aes(x=district,y=0,xend=district,yend=drinking_water, colour="red"))+
  coord_flip()
  geom_hline(yintercept = drinking_water)
  


  coord_flip()+geom_col(fill = 'dodgerblue4') +
  theme_minimal() 





alif$

library(dataxray)
alif |> make_xray() |>
  view_xray()
