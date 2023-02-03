library(tidyverse)
library(fpp3)
library(tseries)
library(lubridate)
library(readxl)
cpi<- read_csv("data/CPI Inflation (Base Year 2015-16).csv")
cpi<-cpi|>na.omit()
cpi|>glimpse()
cpi$infyoy<-parse_number(cpi$`Year on Year`)
cpi$infmom<-parse_number(cpi$`Month over Month`)

cpi<-cpi|>select(Period,infyoy,infmom)
cpi$Date <- dmy(paste("01/", cpi$Period , sep =""))

df <- cpi %>%
    select(Date, infyoy, infmom) %>%
      gather(key = "variable", value = "value", -Date)

ggplot(df, aes(x = Date, y = value)) +
      geom_line(aes(color = variable), size = 2) +
      scale_color_manual(values = c("#00AFBB", "#E7B800")) +
     theme_minimal()+labs(x="Time",y="inflation",title = "Year on year inflation at its peak in history",
                          subtitle="There seems little respite in months to come. Instead of preparing for harsh weathers, we wait for favorable
winds to blow. Inflation, climate, high energy costs, recession in exporting partners among other risks have made Pakistani economy very fragile",
caption="source:karandaz data portal")





bot<- read_csv("data/trade_bal.csv")
bot<-bot|>na.omit()
bot|>glimpse()
bot
bot$export<-parse_number(bot$Exports)
bot$imports<-parse_number(bot$Imports)
bot$trade_balance<-parse_number(bot$`Balance of Trade`)

bot<-bot|>select(Period,infyoy,infmom)
bot$Date <- dmy(paste("01/", bot$Period , sep =""))
bot|>glimpse()|>na.omit()|>
  ggplot(aes(x = Date, y = trade_balance)) +
  geom_line( size = 1, color="red")+theme_minimal()

  scale_color_manual(values = c("#00AFBB")) 
  

df1 <- bot %>%
  select(Date, export, imports) %>%
  gather(key = "variable", value = "value", -Date)
ggplot(df1, aes(x = Date, y = value)) +
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()+labs(x="Time",y="value in millions of USD",title = "Year on year inflation at its peak in history",
                       subtitle="There seems little respite in months to come. Instead of preparing for harsh weathers, we wait for favorable
winds to blow. Inflation, climate, high energy costs, recession in exporting partners among other risks have made Pakistani economy very fragile",
                       caption="source:karandaz data portal")


library(dataxray)
cpi|> select(infyoy,infmom)|>
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
  geom_line(size=1)+geom_rect(aes( ymin=220, ymax=230))
  