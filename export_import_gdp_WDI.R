library(ggthemes)
library(tidyverse)
library(gganimate)
# install.packages("WDI")
# install.packages("RJSONIO")
library(RJSONIO)
library(WDI)

# theme_set(theme_minimal())
# #Let's see if we can find a code for export?
# 
# WDIsearch(string='Export ', field='name', cache=NULL)
# 
# #Ahha, seems like "NE.EXP.GNFS.CD" (Export of Goods and Services) will do it...
# 
# #NE.EXP.GNFS.CD
# 
# ## Similary import is "NE.IMP.GNFS.CD", and gdp "NY.GDP.MKTP.CD@
#
# 
# df_xmgdp <-
#   WDI(
#     country = "all",
#     indicator = c("NE.EXP.GNFS.CD","NY.GDP.MKTP.CD","NE.IMP.GNFS.CD","SP.POP.TOTL"), start=2021,
#     end = 2021
#   )
#   df_xmgdp |> glimpse()
#  save(df_xmgdp,file = "df_xmgdp.RData")  



load("df_xmgdp.RData")

 ## To remove regions
 df_xmgdp |> glimpse()
 


## We have to remove regions from WDI data


library(countrycode)
df_xmgdp %>% 
  as_tibble() %>%
  mutate(region = countrycode(iso2c, "iso2c", "region")) %>% 
  filter(!region %in% c("Sub-Saharan Africa", "Middle East & North Africa") , 
                        !is.na(region)) -> df_wd

df_wd|>  mutate(export=NE.EXP.GNFS.CD/SP.POP.TOTL, 
                import=NE.IMP.GNFS.CD/SP.POP.TOTL,gdp=NY.GDP.MKTP.CD/SP.POP.TOTL)->df_wd
df_wd |> 
  ggplot(aes(x=import,y=gdp)) + geom_point() +
  scale_x_continuous(labels = scales::comma)+scale_y_continuous(labels = scales::comma)+
  geom_text(
    aes(label=country), 
    nudge_x = 0.25, nudge_y = 0.25, 
    check_overlap = T
  )


df_wd |>  
  ggplot(aes(x=import,y=gdp)) + geom_point() +
  scale_y_log10(labels = scales::comma)+scale_x_log10(labels = scales::comma)+
  geom_text(
    aes(label=iso3c), 
    nudge_x = 0.25, nudge_y = 0.25, 
    check_overlap = T
  )


df_wd |>  
  ggplot(aes(x=log(import),y=log(gdp))) + geom_point() +
 # scale_y_log10(labels = scales::comma)+scale_x_log10(labels = scales::comma)+
  geom_text(
    aes(label=country), 
    nudge_x = 0.25, nudge_y = 0.25, 
    check_overlap = T
  )

df_wd |>  
  ggplot(aes(x=import,y=gdp))+
  geom_point()+geom_smooth(method = "lm",se=FALSE)+

geom_point(aes(x =270 , y = 1505,colour="red",size=2))+
  labs(x="Import in billion of USD",y="GDP in billion of USD",title = "GDP and Imports: log-log model", 
       subtitle = "Taking log of variables convert nonlinear relationship to linear relationshi", caption="By Zahid Asghar,data:WDI")+
  geom_text( x =270 , y = 1505, label = "Pakistan")
+
  scale_y_log10()+scale_x_log10()


df_wd |>  
  ggplot(aes(y=import,x=export))+
  geom_point()+geom_smooth(method = "lm",se=FALSE)+
  
  geom_point(aes(x =62.65918 , y = 348.2625,colour="red",size=2))+
  labs(x="Import in billion of USD",y="GDP in billion of USD",title = "GDP and Imports: log-log model", 
       subtitle = "Taking log of variables convert nonlinear relationship to linear relationshi", caption="By Zahid Asghar,data:WDI")+
  geom_text( x =62.65918 , y = 348.2625, label = "Pakistan")
+
  scale_y_log10()+scale_x_log10()

log(62.65918)
l

df2 <-
  df_wd |> filter(import>50 & import<2000, export>50) 
 #country %in% c("Pakistan", "India", "Afghanistan", "Bangladesh", "Sri Lanka","Vietnam"))


df2 |>  
  ggplot(aes(x=import,y=export))+
  geom_point()+geom_smooth(method = "lm",se=FALSE)+

  
  geom_text(label=df2$country,colour="red",size=3)+
  theme_tufte()+ 
  scale_y_log10(labels = scales::comma)+scale_x_log10(labels = scales::comma)+
  labs(title = "Some selected countries having lower per capita export and import",
       subtitle = "Export per capita can provide insights into a country's economic performance alongside other
indicators and factors such as imports, GDP, employment rates, income distribution, and overall economic policies.",
       caption="Source: WDI, Zahid Asghar") +
  theme(plot.title = element_text(size = 15))

df2 |>  
  ggplot(aes(x=import,y=export))+
  geom_point()+geom_smooth(method = "lm",se=FALSE)+
  
  
  geom_text(label=df2$country,colour="red",size=3)+
  theme_tufte()+ 
  scale_y_log10(labels = scales::comma)+scale_x_log10(labels = scales::comma)+
  labs(title = "Pakistan is at the bottom among lower per capita export and import countries",
       subtitle = "Export per capita can provide insights into a country's economic performance alongside other
indicators and factors such as imports, GDP, employment rates, income distribution, and overall economic policies.",
       caption="Source: WDI, Zahid Asghar") +
  theme(plot.title = element_text(size = 15))


plt <- df2 |>  
  ggplot(aes(x=import,y=export))+
  geom_point()+geom_smooth(method = "lm",se=FALSE)+
  
  
  geom_text(label=df2$country,colour="red",size=5)+
  theme_tufte()+ 
  scale_y_log10(labels = scales::comma)+scale_x_log10(labels = scales::comma)+
  labs(title = "Pakistan is at the bottom among lower per capita export and import countries",
       subtitle = "Export per capita can provide insights into a country's economic performance alongside other
indicators and factors such as imports, GDP, employment rates, income distribution, and overall economic policies.",
       caption="Source: WDI, Zahid Asghar") +
  theme(plot.title = element_text(size = 18))



ragg::agg_png("export_per_capita.png", width = 12, height = 12, units = "in", res = 300)
plt
dev.off()





df_wd |>  arrange(export) |> slice(1:100) |> 
  ggplot(aes(x=import,y=export))+
  geom_point()+geom_smooth()+
  
  geom_text(aes(label=country),colour="maroon",size=2.5)+
  theme_tufte()+ 
  scale_y_log10(labels = scales::comma)+scale_x_log10(labels = scales::comma)



df_wd  |> #select(country, export, import, gdp,SP.POP.TOTL) |>arrange(export) |> 
  na.omit() |># top_n(-20,export) |> 
  View()

wdi_72_2021 <- WDI(
      country = "all",
      indicator = c("NE.EXP.GNFS.CD","NY.GDP.MKTP.CD","NE.IMP.GNFS.CD","SP.POP.TOTL"), start=1972,
      end = 2021
    )

save()
wdi_72_2021 |> glimpse()
pk1 <- wdi_72_2021 |> filter(country=="Pakistan")
pk1 |> glimpse()
pak <- full_join(pk,pk1)
pk |> glimpse()
pak |> glimpse()
pak |> as_tibble() |> mutate(export=NE.EXP.GNFS.CD/1000000,gdp=NY.GDP.MKTP.CD/1000000,
                             import=NE.IMP.GNFS.CD/1000000) -> pak
  
save(pak,file="data/pak.RData")
load("data/pak.RData")
ggplot(pak)+aes(x=log(export),y=log(import))+
    geom_point()
  ggplot(pak)+
  geom_line(aes(year,export),size=1,color="blue")+
              geom_line(aes(year,import),size=1, color="maroon")+theme_minimal()+
    #annotate("text", x = 2005, y = 20000, label = "divergence", color = "red", size =5)+
      # labs(title = "IMF SBA is fine but what about root causes",
      #    subtitle = "Trouble emerged around 2005 for whatever reason: China, WTO, Consumption due to USD easy flow or something else")

geom_text(x = 2005, y = 20000, label = "divergence", color="red", size = 15/.pt, inherit.aes = FALSE) +
    labs(title = "IMF SBA is fine but what about root causes") +
    theme(plot.title = element_text(size = 15))
  
plt <-   ggplot(pak)+
  geom_line(aes(year,export),size=1,color="blue")+
              geom_line(aes(year,import),size=1, color="maroon")+theme_minimal()+
    #annotate("text", x = 2005, y = 20000, label = "divergence", color = "red", size =5)+
      # labs(title = "IMF SBA is fine but what about root causes",
      #    subtitle = "Trouble emerged around 2005 for whatever reason: China, WTO, Consumption due to USD easy flow or something else")

geom_text(x = 2005, y = 20000, label = "divergence", color="red", size = 15/.pt, inherit.aes = FALSE) +
    labs(title = "IMF SBA is fine but what about root causes",
         subtitle = "Trouble emerged around 2005 for whatever reason: China, WTO, Consumption led growth due to USD easy flow or something else",
         caption="Source: WDI, Zahid Asghar") +
    theme(plot.title = element_text(size = 15))

  
    ragg::agg_png("export_import.png", width = 12, height = 9, units = "in", res = 300)
  plt
  dev.off()
#
# What is significance of export per capita in economy of a country?
#
# ChatGPT Export per capita is a measure that indicates the value of a country's
# exports divided by its population. It is a useful metric in understanding the
# economic performance and dynamics of a country. Here are a few significances
# of export per capita in the economy of a country:
#
# Economic Growth: A high export per capita generally indicates a strong
# export-oriented economy. It signifies that a country is producing and selling
# goods or services to other nations, which can contribute to economic growth.
# Robust export performance can stimulate domestic industries, create jobs, and
# generate revenue for the country.
#
# Trade Balance: Export per capita is closely related to a country's trade
# balance, which measures the difference between its exports and imports. A
# higher export per capita suggests that a country is exporting more than it is
# importing, leading to a favorable trade balance. A positive trade balance can
# help reduce trade deficits, increase foreign exchange reserves, and improve
# the overall economic stability of a nation.
#
# Foreign Exchange Earnings: Exports are a significant source of foreign
# exchange earnings for a country. Higher export per capita implies greater
# inflows of foreign currency, which can strengthen a country's ability to
# import goods and services, repay external debts, and make international
# investments. Foreign exchange earnings from exports can contribute to the
# stability of a country's currency and its overall financial position.
#
# Industrial Development: A high export per capita often indicates a diverse and
# competitive industrial base. It suggests that a country has developed
# industries and sectors that are capable of producing goods or services that
# are in demand globally. Export-oriented industries tend to invest in
# technology, innovation, and quality improvement to remain competitive in
# international markets. This can lead to the growth and development of domestic
# industries, fostering industrialization and economic diversification.
#
# International Competitiveness: Export per capita is a measure of a country's
# international competitiveness. A higher export per capita suggests that a
# country's products or services are in demand globally and can compete
# effectively with those produced by other nations. To maintain or improve
# export performance, countries often focus on enhancing productivity, quality
# standards, innovation, and marketing strategies, which can drive overall
# economic competitiveness.
#
# It's important to note that while export per capita can provide insights into
# a country's economic performance, it should be considered alongside other
# indicators and factors such as imports, GDP, employment rates, income
# distribution, and overall economic policies. Additionally, the composition of
# exports, such as whether they are primarily raw materials or value-added
# goods, can also impact the significance and implications of export per capita
# in an economy.

#A lower per capita export can have several implications for the people of a country. Here are some ways in which a decrease in per capita export can affect individuals:

# Employment and Income: Lower per capita export can lead to a decline in job
# opportunities in export-oriented industries. When exports decrease, businesses
# may reduce production levels, leading to layoffs or a hiring freeze. This can
# result in increased unemployment rates and reduced income for individuals who
# were previously employed in those sectors. It can also affect related
# industries and sectors that provide goods or services to export-oriented
# industries, further impacting employment and income.
#
# Economic Growth and Development: Per capita export is often associated with
# economic growth and development. A decrease in per capita export can hinder
# economic progress by reducing revenue inflows and limiting investment
# opportunities. Economic growth can be slowed, which may lead to a lower
# standard of living for the population. The availability of public services,
# infrastructure development, and government spending on social programs can
# also be impacted due to reduced revenue from exports.
#
# Poverty and Inequality: Lower per capita export can exacerbate poverty and
# income inequality. When export revenues decrease, it can lead to a reduction
# in government funds available for poverty alleviation programs, social
# welfare, and public services. As a result, vulnerable populations may face
# increased hardship, reduced access to essential services, and limited
# opportunities for economic advancement. Inequality can also widen as those
# dependent on export-related industries may experience greater economic
# vulnerability compared to individuals employed in other sectors.
#
# Trade Balance and Currency Stability: A decrease in per capita export can
# negatively impact a country's trade balance. If exports fall while imports
# remain constant or increase, it can lead to a trade deficit. A sustained trade
# deficit can strain a country's foreign exchange reserves and put pressure on
# its currency. Currency devaluation or depreciation can result in higher import
# costs, leading to increased prices of goods and services. This, in turn, can
# affect the purchasing power of individuals, leading to a decrease in their
# standard of living.
#
# Investment and Innovation: Per capita export often reflects a country's
# ability to attract foreign investment and stimulate domestic innovation. Lower
# export levels can deter foreign investors who seek markets with growth
# potential. Reduced investment can limit job creation, hinder the transfer of
# technology and knowledge, and impede innovation within domestic industries.
# Over time, this can result in a less dynamic and competitive economy,
# affecting the long-term prospects for employment and income growth.
#
# It's important to note that the impact of lower per capita export on
# individuals can vary depending on the country's economic structure, policies,
# diversification of industries, and social safety nets in place. Governments
# can implement measures to mitigate the negative effects, such as diversifying
# the economy, promoting domestic consumption, supporting new industries, and
# investing in education and skills development to enhance competitiveness.
