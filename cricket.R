library(cricketdata)
library(tidyverse)
Azhar_Ali_id<-find_player_id("Azhar Ali")$ID
Azhar_Ali_id
Azhar_Ali<-fetch_player_data(Azhar_Ali_id, "Test") %>%
  mutate(NotOut = (Dismissal == "not out"))

AAave <- Azhar_Ali %>%
  filter(!is.na(Runs)) %>%
  summarise(Average = sum(Runs) / (n() - sum(NotOut))) %>%
  pull(Average)
AAave
names(AAave) <- paste("Average =", round(AAave, 2))
# Plot ODI scores
ggplot(Azhar_Ali) +
  geom_hline(aes(yintercept = AAave), col="gray") +
  geom_point(aes(x = Date, y = Runs, col = NotOut)) +
  ggtitle("Azhar Ali Test Matches Scores") +
  scale_y_continuous(sec.axis = sec_axis(~., breaks = AAave))+theme(legend.position = "none") 

View(Azhar_Ali)
library(help=cricketdata)

Miandad<-fetch_player_data(Javed_Miandad_id, "ODI") %>%
  mutate(NotOut = (Dismissal == "not out"))
head(Miandad)
# Compute batting average
MDave <- Miandad %>%
  filter(!is.na(Runs)) %>%
  summarise(Average = sum(Runs) / (n() - sum(NotOut))) %>%
  pull(Average)
MDave
names(MDave) <- paste("Average =", round(MDave, 2))
# Plot ODI scores
ggplot(Miandad) +
  geom_hline(aes(yintercept = MDave), col="gray") +
  geom_point(aes(x = Date, y = Runs, col = NotOut)) +
  ggtitle("Miandad ODI Scores") +
  scale_y_continuous(sec.axis = sec_axis(~., breaks = MDave))



Dhoni_id<-find_player_id("Dhoni")$ID
Dhoni<-fetch_player_data(Dhoni_id, "ODI") %>%
  mutate(NotOut = (Dismissal == "not out"))
head(Dhoni)
# Compute batting average
DDave <- Dhoni %>%
  filter(!is.na(Runs)) %>%
  summarise(Average = sum(Runs) / (n() - sum(NotOut))) %>%
  pull(Average)
DDave
names(DDave) <- paste("Average =", round(DDave, 2))
# Plot ODI scores
ggplot(Dhoni) +
  geom_hline(aes(yintercept = DDave), col="gray") +
  geom_point(aes(x = Date, y = Runs, col = NotOut)) +
  ggtitle("M S Dhoni ODI Scores") +
  scale_y_continuous(sec.axis = sec_axis(~., breaks = DDave))

Pakbatting <- fetch_cricinfo("Test", "Men", "Batting",country = "Pakistan")
Pakbatting|>glimpse()
Pakfielding <- fetch_cricinfo("Test", "Men", "Fielding",country = "Pakistan")
View(Pakfielding)
Pakfielding|>arrange(desc(Dismissals))
Pakfielding %>%
  mutate(wktkeeper = (CaughtBehind > 0) | (Stumped > 0)) %>%
  ggplot(aes(x = Matches, y = Dismissals, col = wktkeeper)) +
  geom_point() +
  ggtitle("Pakistani Men Test Fielding")+geom_label(x=80,y=200,label="Wasim Bari")
Pakfielding
data<-Pakfielding %>%
  mutate(wktkeeper = (CaughtBehind > 0) | (Stumped > 0)) 
View(data)
ggplot(data,aes(x = Matches, y = Dismissals, col = wktkeeper)) +
  geom_point() +
  ggtitle("Pakistani Men Test Fielding")+ geom_label( 
 data=data %>% filter(Matches>20 & Dismissals>50), # Filter data first
  aes(label=Player)
)

g1<-ggplot(data,aes(x = Matches, y = Dismissals, col = wktkeeper)) +
  geom_point() +
  ggtitle("Pakistani Men Test Fielding")+ geom_label( 
    data=data %>% filter(Matches>40 & Dismissals>200), # Filter data first
    aes(label=Player)
  )

library(plotly)

ggplotly(g1)





psl <- fetch_cricinfo("T20", "Men", "Batting",country = "Pakistan")
psl|>glimpse()



# library
library(ggplot2)
library(dplyr)
library(tibble)

# Keep 30 first rows in the mtcars natively available dataset
data=head(mtcars, 30)
data
# Change data rownames as a real column called 'carName'
data <- data %>%
  rownames_to_column(var="carName")
data
# Plot
ggplot(data, aes(x=wt, y=mpg)) +
  geom_point() + 
  geom_label( 
    data=data %>% filter(mpg>20 & wt>3), # Filter data first
    aes(label=carName)
  )


Mohammad_Rizwan_id<-find_player_id("Mohammad Rizwan")$ID
Mohammad_Rizwan_id
Mohammad_Rizwan<-fetch_player_data(Mohammad_Rizwan_id, "ODI") %>%
  mutate(NotOut = (Dismissal == "not out"))
Mohammad_Rizwan|>glimpse()
MRave <- Mohammad_Rizwan %>%
  filter(!is.na(Runs)) %>%
  summarise(Average = sum(Runs) / (n() - sum(NotOut))) %>%
  pull(Average)
MRave
names(MRave) <- paste("Average =", round(MRave, 2))
# Plot ODI scores
rizwan<-ggplot(Mohammad_Rizwan) +
  geom_hline(aes(yintercept = MRave), col="gray") +
  geom_point(aes(x = Date, y = Runs, col = NotOut)) +
  ggtitle("Mohammad_Rizwan ODI Matches Scores") +
  scale_y_continuous(sec.axis = sec_axis(~., breaks = MRave))+theme(legend.position = "none") 
rizwan

Haris_Sohail_id<-find_player_id("Haris Sohail")$ID
Haris_Sohail_id
Haris_Sohail<-fetch_player_data(Haris_Sohail_id, "ODI") %>%
  mutate(NotOut = (Dismissal == "not out"))
HSave<-Haris_Sohail %>%
  filter(!is.na(Runs)) %>%
  summarise(Average = sum(Runs) / (n() - sum(NotOut))) %>%
  pull(Average)
HSave
names(HSave) <- paste("Average =", round(HSave, 2))
# Plot ODI scores
Haris<-ggplot(Haris_Sohail) +
  geom_hline(aes(yintercept = HSave), col="gray") +
  geom_point(aes(x = Date, y = Runs, col = NotOut)) +
  ggtitle("Haris_Sohail ODI Matches Scores") +
  scale_y_continuous(sec.axis = sec_axis(~., breaks = HSave))+theme(legend.position = "none") 
Haris





Sarfaraz_Ahmed_id<-find_player_id("Sarfaraz Ahmed")$ID


Sarfaraz_Ahmed<-fetch_player_data(Sarfaraz_Ahmed_id, "Test") %>%
  mutate(NotOut = (Dismissal == "not out"))
Sarfaraz_Ahmed|>glimpse()
SAave <- Sarfaraz_Ahmed %>%
  filter(!is.na(Runs)) %>%
  summarise(Average = sum(Runs) / (n() - sum(NotOut))) %>%
  pull(Average)
SAave
names(SAave) <- paste("Average =", round(SAave, 2))
# Plot ODI scores
sarfaraz<-ggplot(Sarfaraz_Ahmed) +
  geom_hline(aes(yintercept = SAave), col="gray") +
  geom_point(aes(x = Date, y = Runs, col = NotOut)) +
  ggtitle("Sarfaraz Ahmed Test Matches Scores") +
  scale_y_continuous(sec.axis = sec_axis(~., breaks = SAave))+theme(legend.position = "none") 
library(patchwork)
rizwan+sarfaraz
Sarfaraz_Ahmed
griz<-Mohammad_Rizwan|>slice_tail(n=20) %>% ggplot()+
  aes(x=Runs)+geom_boxplot()+coord_flip()+ggtitle("Rizwan last 20 innings")
gsarfaraz<-Sarfaraz_Ahmed|>slice_tail(n=20) %>% ggplot()+
  aes(x=Runs)+geom_boxplot()+coord_flip()+labs(title="Sarfaraz last 20 innings", caption="ESPN: By Zahid Asghar")
griz+gsarfaraz

Mohammad_Rizwan|>slice_tail(n=20)|>na.omit()|>summarise(average=mean(Runs))
Sarfaraz_Ahmed|>slice_tail(n=20)|>na.omit()|>summarise(average=mean(Runs))



library(rJava)
df<-fetch_cricsheet(
  type =  "match",
  gender =  "male",
  competition = "psl")

df|>glimpse()
psl_bbb <- fetch_cricsheet(competition = "psl",gender="male", type = "bbb")
psl_match <- fetch_cricsheet(competition = "psl", type = "match")
psl_player <- fetch_cricsheet(competition = "psl",gender = "male", type = "player")
psl_player|>glimpse()
psl_bbb
df |> select(Player, Country, Matches, Runs, Wickets, Economy, StrikeRate)




library(cricketdata)
library(readr)
library(dplyr)
library(stringr)
library(sysfonts)
library(showtext)
#> Loading required package: sysfonts
#> Loading required package: showtextdb
library(ggplot2)
library(gghighlight)
library(ggtext)
library(patchwork)

# Fetch ball-by-ball data
psl_bbb <- fetch_cricsheet(competition = "psl", gender = "male")


# Fetch match metadata
psl_match_info <- fetch_cricsheet(competition = "psl", type = "match", gender = "male")

psl_bbb|>tail()


# Alyssa Healy compared too all players who have batted in 3+ innings in a season.
batting_per_season <- psl_bbb %>%
  group_by(season, striker) %>%
  summarise(
    innings_total = length(unique(match_id)),
    runs_off_bat_total = sum(runs_off_bat),
    balls_faced_total = length(ball),
    .groups = "keep"
  ) %>%
  mutate(
    runs_per_innings_avg = round(runs_off_bat_total / innings_total, 1),
    strike_rate = round(runs_off_bat_total / balls_faced_total * 100, 1)
  ) %>%
  filter(innings_total > 2) %>%
  mutate(is_healy = (striker == "Mohammad Rizwan"))





# Import fonts from Google Fonts
font_add_google("Roboto Condensed", "roboto_con")
font_add_google("Staatliches", "staat")
showtext_auto()

# Build plot
batting_per_season %>%
  ggplot(aes(
    x = season, y = runs_per_innings_avg,
    group = striker, colour = is_healy
  )) +
  geom_line(size = 2, colour = "#F80F61FF") +
  gghighlight(is_healy,
              label_key = striker,
              label_params = aes(
                size = 6, force_pull = 0.1, nudge_y = 10, label.size = 1,
                family = "roboto_con", label.padding = 0.5,
                fill = "#19232FFF",
                colour = "#F80F61FF"
              ),
              unhighlighted_params = list(size = 1, color = "#187999FF")
  ) +
  labs(
    title = "PSL: Average runs scored per innings (3+ innings)",
    x = NULL, y = NULL,
    caption = "**Source:** Cricsheet.org // **courtesy:Jacquie Tran: PSL by Zahid Asghar"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 18, family = "roboto_con", colour = "#FFFFFF"),
    plot.title = element_text(family = "staat", margin = margin(0, 0, 15, 0)),
    plot.caption = element_markdown(size = NULL, margin = margin(15, 0, 0, 0)),
    axis.text = element_text(colour = "#FFFFFF"),
    legend.position = "none",
    panel.grid.major = element_line(linetype = "dashed"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(
      fill = "#171F2AFF", colour = NA
    ),
    panel.spacing = unit(2, "lines"),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
  )

psl_match_info|>glimpse()
psl_match_info <-
  psl_match_info |>  mutate_at(c('winner_wickets', 'winner_runs', 'winner_wickets'), as.numeric)
max(psl_match_info$winner_runs)
max_runs = psl_match_info[which.max(psl_match_info$winner_runs),] 
max_runs %>% select('winner','winner_runs', 'season')

max_wickets = psl_match_info[which.max(psl_match_info$winner_wickets),] 
max_wickets %>% select('winner','winner_wickets', 'season',,'team1', 'team2')





min_runs = psl_match_info[which.min(psl_match_info$winner_runs),] 
min_runs %>% select('winner','winner_runs', 'season','team1', 'team2')




## How many seasons we’ve got in the dataset?
season_count = length(unique(psl_match_info$season))
season_count

##Which Season had most number of matches?
  psl_match_info %>% group_by(season) %>% summarise(match_cnt = n()) %>%
  filter(match_cnt == max(match_cnt))

## Which PSL Team is more successful?  
  psl_match_info %>% group_by(winner) %>% summarise(winner_cnt = n()) %>%
    filter(winner_cnt == max(winner_cnt))
## Barplot for team who won max matches  
success_team = psl_match_info %>% group_by(winner) %>% summarise(winner_cnt = n())
  BP_success_team = ggplot(success_team) + geom_bar(aes(winner,winner_cnt, fill = winner), stat = 'identity') + 
    coord_flip()+labs(title="Number of matches won by each team in PSL")

  BP_success_team

##  Has Toss-winning helped in winning matches?
psl_match_info<-psl_match_info|>mutate(toss_winner=as_factor(toss_winner))
  y=0
 n=0
for(i in seq(1,nrow(psl_match_info))){
    if (psl_match_info$toss_winner[i] == psl_match_info$winner[i])
      y=y+1
    else 
      n=n+1
  }
  
  if (y >= n)
  {
    print(paste("Yes, Toss-winning helped in winning matches."))
    print(paste("Matches won by toss_winner are: ", y, "& Total matches: ", nrow(psl_match_info)))
  } else
    
  {
    print(paste("No, Toss-winning didn't help in winning matches."))
    print(paste("Matches won by other team are: ", n, "& Total matches: ", nrow(psl_match_info))) 
  }

psl_match_info |> group_by(player_of_match) |>
  summarise(awards=n())
psl_match_info |> summarise(awards=n(),.by=player_of_match)|>arrange(desc(awards))

library(ggthemes)
############  IS WINNING TOSS IN PSL HAS ADVANTAGE   ###########################
psl_match_info$toss_match<-ifelse(as.character(psl_match_info$toss_winner)==as.character(psl_match_info$winner),"Won","Lost")
p<-ggplot(psl_match_info[which(!is.na(psl_match_info$toss_match)), ], aes(toss_match, fill = toss_match)) +
  geom_bar() + 
  xlab("Toss") + ylab("Number of matches won") + labs(title =
                                                                     "How much of an advantage is winning the toss in PSL",
                                                                   caption = "cricketdata, By Zahid Asghar")+
  theme_tufte()
p
library(plotly)
ggplotly(p)
