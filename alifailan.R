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

## glimpse()  or str()

## select()

## filter()


## arrange()

## mutate()

## summarise  (mean, min, max, median, sd,q1, q3)



# install.packages("tidyverse")

library(tidyverse)


## .dta , .sav, .xlsx, .csv

library(readr)  # To read csv file




alif <- read_csv("docs/Alifailan.csv", col_names = TRUE)

names(alif)





## Modify column names 
library(janitor)

alif |> clean_names() |> head()    ## data not assigned

alif |> clean_names() -> alif      ## data assigned name alif again

names(alif)





alif |> glimpse()  


glimpse(alif)


head(alif)

tail(alif)


alif |> slice(13:15)



# alif <-   alif |> rename(inf_scr = infrastructure.score,
 #   drink_w = Drinking.water,    bound_w = Boundary.wall,
#    build_cond = Building.condition.satisfactory  )


alif |> filter(province=="ICT")


## Select data for 4 main provinces (two ways either filter or select)

alif |> filter(province != "AJK",
                  province != "GB",
                  province != "ICT",
                  province != "FATA") 

## Or use filter as

alif |> filter (province %in% c("Punjab","Sind","Balochistan", "KP"))

alif |> filter(province==Punjab) ## why an error

alif |> filter(province="Punjab") ## Why an error

alif |> filter(province=="Punjab")


## Select , arrange
alif |> select(district, province, drinking_water)

alif |> select(-district, -rank_2016)

alif |>  count(province, name="district_count")|>
  arrange( desc(district_count)) |> as.data.frame()

 
## How many districts in each province

alif |>  count(province, name="dist_count")

alif |> count(province, name="dist_count") |> 
  arrange(dist_count)


## Summary

alif |> filter(province=="Balochistan") |> 
  select_if(is.numeric) |> select(-rank_2016) |> summarise(mean_drink=mean(drinking_water),
                                                           mean_electiricty=mean(electricity))

## Similarly for Punjab 

# Or if we want for all province at onece 

alif |> group_by(province) |> 
  select_if(is.numeric) |> select(-rank_2016) |> 
  summarise(mean_drink=mean(drinking_water), mean_electiricty=mean(electricity))


## Arrange 

alif |> group_by(province) |> 
  select_if(is.numeric) |> select(-rank_2016) |> 
  summarise(mean_drink=mean(drinking_water), mean_electiricty=mean(electricity)) |> 
  arrange(mean_drink)




library(gt)
library(gtsummary)
library(gtExtras)

alif |> gt_plt_summary()

alif |> select_if(is.numeric) |>  gt_plt_summary()


## If Balochistan

alif |> filter(province=="Balochistan") |> select_if (is.numeric) |> select(-rank_2016) |> 
  gt_plt_summary() |> gt_theme_guardian() |> tab_header(title = "School conditions in Punjab according to Alif Ailan Data")

alif |> count(province, name="dist_count") |> 
  arrange(desc(dist_count)) |> gt() |>
  gt_theme_pff()

alif |> select(province, infrastructure_score) |> 
  tbl_summary(by=province)
 

## Summarise

alif |> select(province,drinking_water) |> 
  summarise(mean=mean(drinking_water), median=median(drinking_water),
            min=min(drinking_water),max=max(drinking_water), sd=sd(drinking_water),
            .by=province)

alif |> select(province,drinking_water) |> 
  summarise(mean=mean(drinking_water), median=median(drinking_water),
            min=min(drinking_water),max=max(drinking_water), sd=sd(drinking_water),
            .by=province) |> 
  gt() |>  gt_theme_538()

alif |> select(province,drinking_water) |> 
  summarise(mean=round(mean(drinking_water),2), median=median(drinking_water),
            min=min(drinking_water),max=max(drinking_water), sd=round(sd(drinking_water),2),
            .by=province) |> 
  gt() |>  gt_theme_nytimes() |> tab_header("Drinking water situation province wise") |> 
  tab_footnote("Source: alifailan, by: Zahid Asghar")

alif |> select(province, district,drinking_water) |> arrange(-drinking_water) |> slice(1)

## plots

library(ggplot2)


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

library(help="janitor")



alif |> filter(province=="Punjab") |> 
  ggplot()+aes(x=drinking_water,y=electricity)+
  geom_point()+geom_text(aes(label=district),hjust=0, vjust=0)

alif |> filter(province=="Punjab") -> alif_punjab
  ggplot(alif_punjab)+aes(x=drinking_water,y=electricity)+
  geom_point()+geom_label(data=alif_punjab |> filter(electricity<70),
                          aes(label=district),hjust=0, vjust=0)
  
ggplot(alif)+
  aes(x=drinking_water,y=electricity)+
  geom_point()+geom_label(data=alif |> filter(province=="Balochistan"&electricity<20),
                          aes(label=district),col='red',size=2, hjust=0,vjust=0)





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
  
#geom_hline(yintercept = drinking_water)
  


  coord_flip()+geom_col(fill = 'dodgerblue4') +
  theme_minimal() 






library(dataxray)
alif |> make_xray() |>
  view_xray()
