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

cpi |> mutate(date=ceiling_date(date,"month")-days(1)) ->cpi


df <- cpi |> 
    select(date, year_on_year, month_over_month) |> 
      gather(key = "variable", value = "value", -date) 

 ggplot(df, aes(x = date, y = value)) +
      geom_line(aes(color = variable), size = 2) + 
      scale_color_manual(values = c("#00AFBB", "#E7B800")) +
     theme_minimal()+labs(x="Time",y="inflation YoY & MoM",title = "Inflation Rate in Pakistan is continuously on rise  and hitting all time high in May-2023.",
                          subtitle="Rural inflation is 42.2% on YoY basis.",
caption="source:PBS, by: Zahid Asghar")

 # Highlight specific months with a different color
 highlighted_months <- c("2022-09-30", "2023-09-30")
 colors <- c("blue", "red", rep("blue", length(df$date) - 2))
 
 df$highlight <- ifelse(df$date %in% highlighted_months, "highlighted", "not highlighted")

inflation <- df |> filter(variable=="year_on_year") |> 
  ggplot()+ aes(x = date, y = value, fill=highlight) +
  geom_col() + 
  #scale_color_manual(values = "#00AFBB") +
  theme_minimal()+labs(x="Time",y="inflation YoY",title = "30% Sep-23 over 23% Sep-22 inflation",
                       subtitle = "This high inflation is simply a killer and more to come by. IMF conditions fulfilment simply means more indirect taxes 
                       without doing anything hard and where it is required. Rural inflation surpassed 33% and all with almost 
                       zero economic growth",
                       caption = "Source: PBS \n Graphic: @zahedasghar")+ theme(legend.position = "none")+
  theme_grey(base_size = 15)

inflation 
 
library(ragg)
ragg::agg_png("images/inflation_15x10.png", width = 15, height = 10, units = "in", res = 300)

inflation
dev.off()



df |> filter(variable=="year_on_year") |> 
  ggplot()+ aes(x = date, y = value, fill="red") +
  geom_line(size=1.5, col="#0b6ba6") + 
  scale_x_date(
    limits = c(as.Date("2020-03-31"), as.Date("2023-06-30")),
    expand = c(0, 0)
  ) + 
  geom_hline(yintercept = 29.2, linetype="dashed", color = "#cf191e",size=1) +
  scale_y_continuous(
    limits = c(5, 45),
    labels = scales::percent_format(scale = 1)
  )+
  geom_hline(yintercept = 0, size = 1.1) + 
  labs(x="",y="",
    title = "Pakistan CPI witnessed little decrease with YoY inflation 28.3% ",
    subtitle = "Bumpy road ahead due to apparent surge in oil prices at global level besides putting extra burden on those who pay their bills regularly",
    caption = "Source: PBS \n Graphic: @zahedasghar"
  ) +
  annotate(
    geom = "curve", x = as.Date('2022-11-30'), y = 32, xend = as.Date('2023-01-31'), yend = 21, 
    curvature = .3, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate('label', x = as.Date('2022-11-30'), y = 32, label = "SBP Nov target 21%", hjust = "right") + 
  annotate('label', x = as.Date('2023-03-31'), y = 26, label = "SBP revised march \n upper limit (29%)")+
  theme_fivethirtyeight() + 
  theme(panel.background = element_rect(fill = "#ffffff"),
        plot.background = element_rect(fill = "#ffffff"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        panel.border = element_blank())+
  theme_grey(base_size = 15)

plot <- df |> filter(variable=="year_on_year") |> 
  ggplot()+ aes(x = date, y = value, fill="red") +
  geom_line(size=1.5, col="#0b6ba6") + 
  scale_x_date(
    limits = c(as.Date("2020-03-31"), as.Date("2023-06-30")),
    expand = c(0, 0)
  ) + 
  geom_hline(yintercept = 29.2, linetype="dashed", color = "#cf191e",size=1) +
  scale_y_continuous(
    limits = c(5, 45),
    labels = scales::percent_format(scale = 1)
  )+
  geom_hline(yintercept = 0, size = 1.1) + 
  labs(x="",y="",
       title = "Annual consumer price inflation was 29.2% in FY-2023",
       subtitle = "After January, YoY inflation first time touching below 30%. MoM is 4th time negative in last 3 years. \n Core inflation is still very high so seems no immediate respite other than base effect ",
       caption = "Source: PBS \n Graphic: @zahedasghar"
  ) +
  annotate(
    geom = "curve", x = as.Date('2022-11-30'), y = 32, xend = as.Date('2023-01-31'), yend = 21, 
    curvature = .3, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate('label', x = as.Date('2022-11-30'), y = 32, label = "SBP Nov target 21%", hjust = "right") + 
  annotate('label', x = as.Date('2023-03-31'), y = 26, label = "SBP revised march \n upper limit (29%)")+
  theme_fivethirtyeight() + 
  theme(panel.background = element_rect(fill = "#ffffff"),
        plot.background = element_rect(fill = "#ffffff"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        panel.border = element_blank())+
  theme_grey(base_size = 15)


ragg::agg_png("images/inflation_15x10.png", width = 15, height = 10, units = "in", res = 300)
plot
dev.off()





  #scale_color_manual(values = "#00AFBB") +
  theme_minimal()+labs(x="Time",y="inflation YoY",title = "Inflation Rate in Pakistan is continuously on rise  and hitting all time high in May-2023.",
                       subtitle="Rural inflation is 42.2% on YoY basis.",
                       caption="source:PBS, by: Zahid Asghar")+ theme(legend.position = "none")


df |> filter(variable=="month_over_month") |> 
  ggplot()+ aes(x = date, y = value, fill="red") +
  geom_line(size=1, col="red") + 
  #scale_color_manual(values = "#00AFBB") +
  theme_minimal()+labs(x="Time",y="inflation MoM",title = "Inflation Rate in Pakistan is continuously on rise  and hitting all time high in May-2023.",
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


df |> mutate(color = case_when(value < 15 ~ "red",
                                      TRUE ~ "dodgerblue")) |>
  filter(variable == "year_on_year") |>
  ggplot() + aes(x = date, y = value, fill = "red") +
  geom_bar(aes(fill = value < 15), stat = 'identity') +
  theme_minimal() + labs(
    x = "Time",
    y = "inflation",
    title = "27.3% is very high inflation as base rate was also very high",
    subtitle = "Inflation has become very serious as 27% inflation over last three months have a very high base. \n Hike in fuel prices and recovering line losses from those who are regularly paying their bills will make things \n more worse in months to come",
    caption = "source:PBS, by: Zahid Asghar" 
  ) + theme(legend.position = "none")+theme(plot.title = element_text(size = 15))

camcorder::gg_record(
  dir = 'images',
  width = 12,
  height = 12 * 9 / 16,
  dpi = 300,
  bg = 'white' 
  # Makes sure background of plot is actually white, not transparent
)


ggplotly(p11)






p <- df |> filter(variable=="year_on_year") |> 
  ggplot()+ aes(x = date, y = value, fill="red") +
  geom_col() + 
  #scale_color_manual(values = "#00AFBB") +
  theme_minimal()+labs(x="Time",y="inflation",title = "Inflation hit a record high of 35.4 percent in March since 1965 ",
                       subtitle="No favorable winds seem blowing at least in the coming couple of month. \n Inflation, climate, high energy costs, recession in exporting partners among \n other risks have made Pakistani economy very fragile",
                       caption="source:PBS, by: Zahid Asghar")+theme(legend.position="none")


p
library(plotly)

ggplotly(p)
library(magick)
gif<-image_read("https://img.etimg.com/thumb/msid-93194137,width-1200,height-900,imgsize-1101135,resizemode-8/20220729_inflation_01.jpg")

scaled_gif <- image_scale(gif, "300x200") 
plot <- image_graph(width = 800, height = 700, res = 100)
inflation
dev.off()
frames <- image_composite(plot, scaled_gif, offset = "+300-200")
frames
animation <- image_animate(image_join(frames), optimize = TRUE)
animation <- image_quantize(animation,
                            max = 20,
                            colorspace = "rgb")

animation


image_write(animation, "C:D:\\RepTemplates\\data_analytics\\infation.gif")

animation <- image_animate(image_join(frames), optimize = TRUE, loop = 1)

animation <- image_quantize(animation,
                            max = 20,
                            colorspace = "rgb")
length(gif)
gif<-gif|>image_scale("300")
gif
  