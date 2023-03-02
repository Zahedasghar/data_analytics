# Fetch all Australian Men's ODI data by innings
library(cricketdata)
library(tidyverse)
menODI <- fetch_cricinfo("ODI", "Men", "Batting", type="innings",country = "Pakistan")
menODI|>glimpse()
View(menODI)


Pak_NZ<-menODI|>filter(Opposition=="New Zealand"&Ground=="Karachi")
Pak_NZ$Innings
Pak_NZ|>group_by(Player)|>summarise(total_score=sum(Runs))|>arrange(desc(total_score))

Pak_NZ |> filter(Runs > 0) |> group_by(Date) |> summarise(total_score = sum(Runs)) |>
  arrange(desc(desc(Date)))
Pak_NZ %>%
  ggplot(aes(y = Runs, x = Date)) +
  geom_point(alpha = 0.2, col = "#E97F09") +
  geom_smooth() +
  ggtitle("Pakistani Men: Runs per Innings")



test <- fetch_cricinfo("ODI", "Men",type = "innings", country = "Pakistan")

View(test|>glimpse())

Pak_NZ|>group_by(Date,Innings)|>summarise(total=sum(Innings))


Pak<-fetch_cricinfo(matchtype = "Test","Men","Batting",type="career",country = "Pakistan")
Pak|>glimpse()


# Fetch match metadata
match_info <- fetch_cricsheet(competition = "test", type = "match", gender = "male")
