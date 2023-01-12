library(haven)
library(tidyverse)

#income <- read_dta("D:/HIES 2018-19/data_in_stata/sec_12a.dta")
income <- read_dta("D:/HIES_2019/data_in_stata/sec_12a.dta")
income %>% sjPlot::view_df()
income %>% glimpse()
income %>% filter(s12aq08>0) %>% glimpse()
income<-income %>% filter(s12aq08>0)%>% mutate(loginc=log(s12aq08))

income<-income %>% mutate(province=as.factor(province),region=as.factor(region))

ggplot(income)+aes(s12aq08)+geom_histogram(stat_bin=50)

ggplot(income)+aes(loginc)+geom_histogram()
#ggplot(income)+aes(x=province,y=s12aq08)+geom_boxplot()+
# facet_grid(~province,scale="free")+geom_point(position=position_jitterdodge())
#View(income)

library(kableExtra)
sum_stat<-income  %>% 
  select(s12aq08,province) %>% 
  group_by(province) %>% 
  summarise(mean=mean(s12aq08),
            sd=sd(s12aq08),min=min(s12aq08),median=median(s12aq08),q25 = quantile(s12aq08, 0.25),
            q75 = quantile(s12aq08, 0.75),max=max(s12aq08)
  ) 

sum_stat
sum_stat %>% gt() %>% gt_theme_538() %>% tab_header(title="Summary statistics of income distribution province wise")
sum_stat %>% kable()
## gt table
library(gtsummary)
library(gtExtras)
income  %>% 
  select(loginc,province) %>% 
  group_by(province) %>% 
  summarise(obs=count(),mean=mean(loginc),
            sd=sd(loginc),min=min(loginc),median=median(loginc),q25 = quantile(loginc, 0.25),q75 = quantile(loginc, 0.75),max=max(loginc),
  ) 


income %>% count(province)

library(vtable)
income%>% sjPlot::view_df()
income  %>% 
  select(loginc,province) %>% 
  group_by(province) %>% 
  summarise(mean=mean(loginc),
            sd=sd(loginc),min=min(loginc),median=median(loginc),q25 = quantile(loginc, 0.25),q75 = quantile(loginc, 0.75),max=max(loginc),
  ) %>% gt()

income  %>% 
  select(loginc,province) %>% 
  group_by(province) %>% 
  summarise(mean=mean(loginc),
            sd=sd(loginc),min=min(loginc),median=median(loginc),q25 = quantile(loginc, 0.25),q75 = quantile(loginc, 0.75),max=max(loginc),
  ) %>% gt()



#qplot(log(Totalincome),data=income,color=province,binwidth=0.05,geom="density")
#ggplot(income)+aes(x=province,y=loginc)+geom_boxplot(fill="chartreuse4")+
# facet_wrap(~province)+geom_jitter(alpha=0.5, aes(color=province),position = position_jitter(width = .1))


ggplot(income)+ aes(x=province, y=loginc, fill=province, colour=province) +
  geom_point(position = position_jitterdodge()) +
  geom_boxplot(alpha=0.6)

ggplot(income, aes(x=province, y=loginc,fill=factor(province),colour=factor(province))) +
  geom_jitter() +
  geom_boxplot(alpha=0.4)

ggplot(income)+aes( x=loginc,colour=province) + 
  geom_density()
ggplot(income) + aes(x=loginc, group=province,colour=province,
                     fill=province,alpha=0.5, adjust=2) + geom_density()+
  xlab("Log of income") +
  ylab("Density")
library(ggthemes)
ggplot(income) + aes(x=loginc, group=province,colour=province,fill=province,
                     alpha=0.5, adjust=2) + geom_density()+
  xlab("Log of income") +
  ylab("Density")+ggtitle("income distribution")+
  theme_tufte()
