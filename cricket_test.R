# Fetch all Australian Men's ODI data by innings
library(cricketdata)
library(tidyverse)
menODI <- fetch_cricinfo("ODI", "Men", "Batting", type="innings",country = "Pakistan")
menODI|>glimpse()
View(menODI)


Pak_NZ <- menODI |> filter(Opposition == "New Zealand" &
                             Ground == "Karachi")
Pak_NZ |> View()

library(gt)
library(gtExtras)

Pak_NZ|>group_by(Player)|>summarise(total_score=sum(Runs))|>arrange(desc(total_score)) |>
  filter(total_score>100) |> gt() |> gt_theme_538() |> tab_header(title="Top scoring players against New Zealand in Karachi Stadium") |> 
  tab_footnote("source: cricketdata")

Pak_NZ |> filter(Runs > 0) |> group_by(Date) |> summarise(total_score = sum(Runs)) |>
  arrange(desc(desc(Date)))
Pak_NZ %>% filter(Runs>50, Date>2006-01-01) |> 
  ggplot(aes(y = Runs, x = Date)) +
  geom_point(alpha = 0.2, col = "red",size=4) +
  ggtitle("Pakistani players scored more than 50 in ODI against New Zealand \n in Karachi")+
  geom_text(aes(label=Player),nudge_x = 0.25, nudge_y = 0.25, 
             check_overlap = T)+theme_fivethirtyeight()



ODI <- fetch_cricinfo("ODI", "Men",type = "innings", country = "Pakistan")

ODI |>glimpse()

Pak_NZ|>group_by(Date,Innings)|>summarise(total=sum(Innings))


Pak<-fetch_cricinfo(matchtype = "ODI","Men","Batting",type="career",country = "Pakistan")
Pak|>glimpse()


# Fetch match metadata
match_info <- fetch_cricsheet(competition = "ODI", type = "match", gender = "male")
