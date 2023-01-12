library(WDI)
library(tidyverse)
library(lubridate)
library(tseries)
library(gt)
library(gtExtras)
#NY.GDP.MKTP.KD.ZG
#gdp_growth <- WDI(indicator = "NY.GDP.MKTP.KD.ZG", start = 1975, end = 2021) # gdp growth rate 
saveRDS(gdp_growth,file ="gdp_growth1.rds" )
gdp_growth<-readRDS("gdp_growth.rds")

gdp_growth_sub<-gdp_growth|>filter(country %in%c("Pakistan", "India", "Bangladesh"),year>1990)
gdp_growth_sub|>glimpse()
gdp_growth_sub<-gdp_growth_sub|>rename(gr_rate="NY.GDP.MKTP.KD.ZG")

gdp_growth$year<-as_date(gdp_growth$year)
gdp_growth<-ts(gdp_growth,start = 1975, end=2021)
gdp_growth<-gdp_growth|>arrange(desc())

ggplot(gdp_growth_sub)+aes(x=year,y=gr_rate,color=country)+geom_line()+
  labs(x="year",y="Economic growth rate", title="Economic growth rate from 1991-2021 for \n BD, Indo-Pak",
       caption = "Source: WDI, By: Zahid Asghar")

tab1<-gdp_growth_sub|>select(gr_rate,country)|>group_by(country)|>summarise(avg=round(mean(gr_rate),1))

tab2<-gdp_growth_sub|>filter(year>2011)|>select(gr_rate,country)|>group_by(country)|>
  summarise(avg=round(mean(gr_rate),1))

tab<-bind_cols(tab1,tab2)
tab<-tab|>select(country...1,avg...2,avg...4)

tab<-tab|>rename(country="country...1",avg_1991_2021="avg...2",avg_gr_2012_2021="avg...4")
tab
gdp_per_capita<-c(2227,1942,1547)
tab<-bind_cols(tab,gdp_per_capita)
tab<-tab|>rename(gdp_percap="...4")
tab|>
  gt()|>gt_theme_538()|>tab_header("Economic growth rate compounding brings huge differences")



library(patchwork)
tab1+tab2
