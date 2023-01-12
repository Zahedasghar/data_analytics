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
Mohammad_Rizwan<-fetch_player_data(Mohammad_Rizwan_id, "Test") %>%
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
  ggtitle("Mohammad_Rizwan Test Matches Scores") +
  scale_y_continuous(sec.axis = sec_axis(~., breaks = MRave))+theme(legend.position = "none") 

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

