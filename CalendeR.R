# The palette with grey:
cal_palette <- c("#45FF50", "#7844F6","#FF9999")
library(tidyverse)

library(lubridate)


library(calendR)
start_day <- as.Date("2023-01-01")
end_day <- as.Date("2023-12-31")

df <- tibble(date = seq(start_day, 
                        end_day, by="1 day")) 
holidays <- c(as.Date("2023-05-05"),
              as.Date("2023-03-23"),
              as.Date("2023-04-22"),
              as.Date("2023-04-23"),
              as.Date("2023-04-24"),
              as.Date("2023-06-29"),
              as.Date("2023-06-30"),
              as.Date("2023-07-28"),
              as.Date("2023-07-29"),
              as.Date("2023-09-28"),
              as.Date("2023-09-04"),
              as.Date("2023-11-23"),
              as.Date("2023-11-24"),
              as.Date("2023-12-25"))
calendar_df <- df %>%
  mutate(Year=lubridate::year(date),
         Month = lubridate::month(date, label=TRUE, abbr=FALSE),
         Day = lubridate::wday(date, label=TRUE),
         mday = lubridate::mday(date),
         Month_week = (5 + day(date) +
                         wday(floor_date(date, 'month'))) %/% 7) %>%
  mutate(schedule = case_when(
    Day %in% c("Sat", "Sun") ~ "Week  end",
    date %in% holidays ~ "Holiday",
    TRUE~ "Available")) 


calendar_df %>%  
  ggplot(aes(y=Month_week, x=Day, fill=schedule))+
  geom_tile(color="black")+
  geom_text(aes(label=mday))+
  facet_wrap(~Month, ncol=3)+
  theme_classic(16)+
  scale_y_continuous(breaks = scales::breaks_pretty(n=5))+
  scale_y_reverse()+
  theme(legend.position="bottom")+
  scale_fill_discrete(name=NULL)+
  scale_fill_manual(values=cal_palette)+
  labs(title="2023 Calendar",y="Week",
       x=NULL)
ggsave("make_calender_with_ggplot2.png", width=9, height=12)
