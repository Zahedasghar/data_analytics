library(tidyverse)
library(fpp3)
library(tseries)
library(lubridate)
library(ggthemes)
cpi<- readr::read_csv("data/cpi.csv") ## karandaz march
cpi <- cpi |> na.omit()
cpi |> glimpse()

library(janitor)

cpi <- cpi |> clean_names()
cpi <-
  cpi |>  mutate(year_on_year = year_on_year * 100,
                 month_over_month = month_over_month * 100)
cpi <- cpi |> select(period, year_on_year, month_over_month)

cpi$date <- dmy(paste("01/", cpi$period , sep =""))

cpi

df <- cpi |> 
    select(date, year_on_year, month_over_month) |> 
      gather(key = "variable", value = "value", -date)

ggplot(df, aes(x = date, y = value)) +
      geom_line(aes(color = variable), size = 2) + 
      scale_color_manual(values = c("#00AFBB", "#E7B800")) +
     theme_minimal()+labs(x="Time",y="inflation",title = "YoY inflation at its peak in history",
                          subtitle="A little monthly respite trend changing again. Instead of preparing for harsh weathers, we wait for favorable
winds to blow. Inflation, climate, high energy costs, recession in exporting partners among other risks have made Pakistani economy very fragile",
caption="source:PBS, by: Zahid Asghar")





bot<- read_csv("data/trade_bal.csv")
bot<-bot|>na.omit()
bot <- bot |> clean_names()
bot
bot <- bot |> mutate_at(c('exports', 'imports', 'balance_of_trade'), as.numeric)
#bot$imports<-parse_number(bot$Imports)
#bot$trade_balance<-parse_number(bot$`Balance of Trade`)

#bot<-bot|>select(Period,year_on_year,month_over_month)
bot$date <- dmy(paste("01/", bot$period , sep =""))
bot|>glimpse()|>na.omit()|>
  ggplot(aes(x = date, y = balance_of_trade)) +
  geom_line( size = 1, color="red")+theme_minimal()+

  scale_color_manual(values = c("#00AFBB")) 
  

df1 <- bot %>%
  select(date, exports, imports) %>%
  gather(key = "variable", value = "value", -date)
ggplot(df1, aes(x = date, y = value)) +
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()+labs(x="Time",y="value in millions of USD",title = "Year on year inflation at its peak in history",
                       subtitle="There seems little respite in months to come. Instead of preparing for harsh weathers, we wait for favorable
winds to blow. Inflation, climate, high energy costs, recession in exporting partners among other risks have made Pakistani economy very fragile",
                       caption="source:karandaz data portal")


library(dataxray)
cpi|> select(year_on_year,month_over_month)|>
  make_xray()|>
  view_xray()

library(readxl)
USD_PKR <- read_excel("C:/Users/92300/OneDrive - Higher Education Commission/USD_PKR.xlsx")
USD_PKR|>glimpse()
#USD_PKR$Date<- dmy(paste("01-", USD_PKR$time , sep =""))
View(USD_PKR)
ER<-USD_PKR|>
separate(
  col = `US Dollar to Pakistani Rupee`,
  into = c("first","second","third", "D_Rs"),
  sep = " ",
  remove = FALSE
)  
ER$D_Rs<-as.numeric(ER$D_Rs)
ER<-ER|>select(time, D_Rs)
View(ER)
ggplot(ER, aes(x = time, y = D_Rs)) +
  geom_line(size=1)
+geom_rect(aes( ymin=220, ymax=230))
  