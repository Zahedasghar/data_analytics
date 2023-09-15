library(cricketdata)
library(tidyverse)
#bowling <- fetch_cricinfo("ODI", "Men", "Bowling")
Pakbowling <- fetch_cricinfo("ODI", "Men", "Bowling",country = "Pakistan")
Pakbowling|>glimpse()

pkbowling <- Pakbowling %>%  
  filter(Wickets >= 80) %>%  
  ggplot(aes(x=reorder(Player, Wickets),y=Wickets))+geom_bar(stat="identity", fill="dodgerblue4") +coord_flip()+
  labs(x="",y="Wickets", title="ODI wicket takers club ",
       caption="cricketdata, graphics: @zahedasghar")+theme_minimal()+theme(plot.title = element_text(size = 15))
pkbowling
  
library(ragg)
ragg::agg_png("bowling_ragg_15x15.png", width = 15, height = 15, units = "in", res = 300, scaling = 2)
pkbowling
dev.off()

Pakbowling %>%  select(Player, Matches, Runs, Wickets,StrikeRate) |> 
  filter(Wickets >= 100) %>%  gt()

plt <- Pakbowling %>%  
  filter(Wickets >= 100) %>%  
  ggplot(aes(x=reorder(Player, Wickets),y=Wickets))+geom_bar(stat="identity", fill="dodgerblue4") +coord_flip()+
  labs(x="",y="Wickets", title="80 or more ODI wicket takers ",
       caption="cricketdata, graphics: @zahedasghar")+theme_minimal()+theme(plot.title = element_text(size = 15))

plt  
p1 <-  plt+ 
  theme_grey(base_size = 14) +
  theme(plot.title = element_text(size = rel(1.1)))
p1
p2 <- p1+scale_y_continuous(expand = expansion(mult = c(0, 0.01)))

p2

p3 <- p2+
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
  )
p3

counts_wickets <- Pakbowling |> filter(Wickets>=100) |> group_by(Player) |> count(Wickets)

p4 <- p3+geom_text(
  data = counts_wickets,
  mapping = aes(x = Player, y = Wickets, label = Wickets),
  hjust = 2,
  nudge_x = 0,
  color = 'white'
)+
  geom_vline(xintercept = 0) +
  scale_y_continuous(breaks = NULL, expand = expansion(mult = c(0, 0.01))) +
  labs(y = element_blank()) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())
p4




Pakbowling %>%  
  filter(Wickets >= 100) |> 
  ggplot(aes(y = StrikeRate, x = Average)) +
  geom_point(alpha = 0.3, col = "blue") +
  ggtitle("Test International Pakistani Bowlers") +
  ylab("Balls per wicket") + xlab("Runs per wicket")+geom_label(aes(label=Player))
