library(tidyverse)
library(fpp3)
library(tseries)
library(lubridate)
library(ggthemes)
library(readr)
cpi<- readr::read_csv("data/cpi.csv") 
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
     theme_minimal()+labs(x="Time",y="inflation YoY & MoM",title = "Inflation Rate in Pakistan is continuously on rise  and hitting all time high in May-2023.",
                          subtitle="Rural inflation is 42.2% on YoY basis.",
caption="source:PBS, by: Zahid Asghar")


df |> filter(variable=="year_on_year") |> 
  ggplot()+ aes(x = date, y = value, fill="red") +
  geom_col() + 
  #scale_color_manual(values = "#00AFBB") +
  theme_minimal()+labs(x="Time",y="inflation YoY",title = "Inflation Rate in Pakistan is continuously on rise  and hitting all time high in May-2023.",
                       subtitle="Rural inflation is 42.2% on YoY basis.",
                       caption="source:PBS, by: Zahid Asghar")+ theme(legend.position = "none")




bot <- read_csv("data/trade_bal.csv")
bot <- bot |> na.omit()
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


p <- df |> filter(variable=="year_on_year") |> 
  ggplot()+ aes(x = date, y = value, fill="red") +
  geom_col() + 
  #scale_color_manual(values = "#00AFBB") +
  theme_minimal()+labs(x="Time",y="inflation",title = "Inflation hit a record high of 35.4 percent in March since 1965 ",
                       subtitle="No favorable winds seem blowing at least in the coming couple of month. \n Inflation, climate, high energy costs, recession in exporting partners among \n other risks have made Pakistani economy very fragile",
                       caption="source:PBS, by: Zahid Asghar")+theme(legend.position="none")


p
library(magick)
gif<-image_read("https://img.etimg.com/thumb/msid-93194137,width-1200,height-900,imgsize-1101135,resizemode-8/20220729_inflation_01.jpg")
plot <- image_graph(width = 1000, height = 500, res = 100)
p
dev.off()
frames <- image_composite(plot, gif, offset = "+500-100")
frames
animation <- image_animate(image_join(frames), optimize = TRUE)
animation <- image_quantize(animation,
                            max = 20,
                            colorspace = "rgb")

animation


image_write(animation, "C:D:\\RepTemplates\\data_analytics\\bleeding_econ.gif")

animation <- image_animate(image_join(frames), optimize = TRUE, loop = 1)

animation <- image_quantize(animation,
                            max = 20,
                            colorspace = "rgb")
length(gif)
gif<-gif|>image_scale("300")
gif
  