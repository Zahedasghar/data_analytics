library(tidyverse)
library(readxl)
library(janitor)
library(ggthemes)

gdppk <- read_excel("gdp_growth.xlsx") |> clean_names()

# gdp |> mutate(year=dmy(date))
# gdp |> mutate(year=as_date(date)) -> gdppk

ggplot(gdppk) +
  aes(x=date, y=growth) + 
  geom_line(size=1.2) +
  geom_smooth(size=2, se=FALSE, color="red") +
  theme_minimal()+
  geom_rect(aes(xmin = 1972, xmax = 1977, ymin = -Inf, ymax = Inf), fill = "red", alpha = 0.002) +
  geom_rect(aes(xmin = 1978, xmax = 1985, ymin = -Inf, ymax = Inf), fill = "orange", alpha = 0.002) +
  geom_rect(aes(xmin = 1986, xmax = 1988, ymin = -Inf, ymax = Inf), fill = "steelblue", alpha = 0.002) +
  geom_rect(aes(xmin = 1989, xmax = 1999, ymin = -Inf, ymax = Inf), fill = "red", alpha = 0.002) +
  geom_rect(aes(xmin = 1999, xmax = 2007, ymin = -Inf, ymax = Inf), fill = "blue", alpha = 0.002) +
  geom_rect(aes(xmin = 2008, xmax = 2012, ymin = -Inf, ymax = Inf), fill = "orange", alpha = 0.002) +
  geom_rect(aes(xmin = 2013, xmax = 2017, ymin = -Inf, ymax = Inf), fill = "steelblue", alpha = 0.002) +
  geom_rect(aes(xmin = 2018, xmax = 2022, ymin = -Inf, ymax = Inf), fill = "orange", alpha = 0.002) +
  geom_rect(aes(xmin = 2022, xmax = 2023, ymin = -Inf, ymax = Inf), fill = "steelblue", alpha = 0.002) +
  geom_text(aes(x = 1972, y = max(growth), label = "PPP"), color = "black", vjust = 8, hjust = -0.3, size = 3.5) +
  
  geom_text(aes(x = 1978, y = max(growth), label = "Zia_ML"), color = "red", vjust = 8, hjust = -0.1, size = 3.5) +
  geom_text(aes(x = 1986, y = max(growth), label = "Junejo"), color = "black", vjust = 8, hjust = 0.2, size = 3.5)+
  
  geom_text(aes(x = 1989, y = max(growth), label = "PPPP/PMLN"), color = "black", vjust = 12, hjust = -0.1, size = 3.5)+
  
  geom_text(aes(x = 1999, y = max(growth), label = "Musharaf_PMLQ"), color = "red", vjust = 8, hjust = -0.1, size = 3.5)+
  
  geom_text(aes(x = 2008, y = max(growth), label = "PPPP"), color = "black", vjust = 12, hjust = -0.1, size = 3.5)+
  
  geom_text(aes(x = 2013, y = max(growth), label = "PMLN"), color = "orange", vjust = 8, hjust = -0.1, size = 3.5)+
  
  geom_text(aes(x = 2018, y = max(growth), label = "PTI"), color = "black", vjust = 12, hjust = -0.1, size = 3.5)+
  
  geom_text(aes(x = 2022, y = max(growth), label = "PDM"), color = "black", vjust = 8, hjust = -0.1, size = 3.5)+
  labs(x=element_blank(),title = "Pakistan economic growth rate 1972-2022",
       subtitle = "Pakistan economic growth rate has never been very encouraging irrespective whosoeve has been in the government from 1990s onward",
       caption = "Source: Pak Eco Sur, by Zahid Asghar")+
  theme_tufte()
  



gdppk
