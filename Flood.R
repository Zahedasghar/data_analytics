library(readxl)
flood_damage <- read_excel("flood_damage_28082022.xlsx")
View(flood_damage)
library(tidyverse)
df_flood<-as_tibble(flood_damage)
#df_flood
#View(df_flood)
#df_flood %>% select(Category %in% c(M_death,F_deaths, C_deaths))
#df_flood %>% select(Category %in% c(M_deaths,F_deaths, C_deaths))
#df_flood %>% filter(Category %in% c(M_deaths,F_deaths, C_deaths))
#View(df_flood)
#df_flood %>% filter(Category %in% c("M_deaths","F_deaths", "C_deaths"))
#df_flood %>% filter(Category %in% c("M_deaths","F_deaths", "C_deaths")) %>% group_by(Region)
#df_flood %>% filter(Category %in% c("M_deaths","F_deaths", "C_deaths")) %>% group_by(Region) %>% summarize(total_death=sum(Number))
#df_flood %>% filter(Category %in% c("M_deaths","F_deaths", "C_deaths")) %>% group_by(Region) %>% summarize(total_deaths=sum(Number)) %>%
#  ggplot()+aes(x=Region,y=total_deaths)+geom_bar(stat="identity")
#df_flood %>% filter(Category %in% c("M_deaths","F_deaths", "C_deaths")) %>% group_by(Region) %>% summarize(total_deaths=sum(Number)) %>%
#  ggplot()+aes(x=reorder(Region,total_deaths),y=total_deaths)+geom_bar(stat="identity")
#1<-grdf_flood %>% filter(Category %in% c("M_deaths","F_deaths", "C_deaths")) %>% group_by(Region) %>% summarize(total_deaths=sum(Number)) %>%
#  ggplot()+aes(x=reorder(Region,total_deaths),y=total_deaths)+geom_bar(stat="identity")
gr1<-grdf_flood %>% filter(Category %in% c("M_deaths","F_deaths", "C_deaths")) %>% group_by(Region) %>% summarize(total_deaths=sum(Number)) %>%
  ggplot()+aes(x=reorder(Region,total_deaths),y=total_deaths)+geom_bar(stat="identity")
gr1<-df_flood %>% filter(Category %in% c("M_deaths","F_deaths", "C_deaths")) %>% group_by(Region) %>% summarize(total_deaths=sum(Number)) %>%
  ggplot()+aes(x=reorder(Region,total_deaths),y=total_deaths)+geom_bar(stat="identity")
gr1+xlab("Group") +
  ylab("Value") +
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.5))
gr1+xlab("Group") +
  ylab("Value") +
  theme(axis.text.x = element_text(
    hjust = 1,
    vjust = 0.5))
gr1+xlab("") +
  ylab("Value") +
  theme(axis.text.x = element_text(
    hjust = 1,
    vjust = 0.5))
gr1+geom_text(aes(label = round(value, 1),
                  angle = 90,
                  hjust = ifelse(value < 0, 1.25, -0.25),
                  vjust = 0.5),
              size = 3)+xlab("") +
  ylab("Value") +
  theme(axis.text.x = element_text(
    hjust = 1,
    vjust = 0.5))
gr1+geom_text(aes(label = total_deaths,
                  angle = 90,
                  hjust = ifelse(value < 0, 1.25, -0.25),
                  vjust = 0.5),
              size = 3)+xlab("") +
  ylab("Value") +
  theme(axis.text.x = element_text(
    hjust = 1,
    vjust = 0.5))
gr1+geom_text(aes(label = total_deaths,
                  angle = 90,size = 3))+xlab("") +
  ylab("Value") +
  theme(axis.text.x = element_text(
    hjust = 1,
    vjust = 0.5))
gr1+geom_text(aes(label = total_deaths,
                  angle = 0,size = 3))+xlab("") +
  ylab("Value") +
  theme(axis.text.x = element_text(
    hjust = 1,
    vjust = 0.5))
gr1+geom_text(aes(label = total_deaths,
                  angle = 0,vjust = 0.5,size = 3))+xlab("") +
  ylab("Value") +
  theme(axis.text.x = element_text(
    hjust = 1,
    vjust = 0.5))
gr1+geom_text(aes(label = total_deaths,
                  angle = 0,vjust = 1,size = 3))+xlab("") +
  ylab("Value") +
  theme(axis.text.x = element_text(
    hjust = 1,
    vjust = 0.5))
gr1+geom_text(aes(label = total_deaths,
                  angle = 0,vjust = .1,size = 3))+xlab("") +
  ylab("Value") +
  theme(axis.text.x = element_text(
    hjust = 1,
    vjust = 0.5))
gr1+geom_text(aes(label = total_deaths,
                  angle = 0,vjust = .1,size = 3))+xlab("") +
  ylab("Value") +
  theme(axis.text.x = element_text(
    hjust = 1,
    vjust = 0.5))+ggthemes::wsj_pal()
library(ggthemes)
gr1+geom_text(aes(label = total_deaths,
                  angle = 0,vjust = .1,size = 3))+xlab("") +
  ylab("Value") +
  theme(axis.text.x = element_text(
    hjust = 1,
    vjust = 0.5))+theme_solarized()
gr1+geom_text(aes(label = total_deaths,
                  angle = 0,vjust = .1))+xlab("") +
  ylab("Value") +
  theme(axis.text.x = element_text(
    hjust = 1,
    vjust = 0.5))+theme_solarized()
gr1<-df_flood %>% filter(Category %in% c("M_deaths","F_deaths", "C_deaths")) %>% group_by(Region) %>% summarize(total_deaths=sum(Number)) %>%
  ggplot()+aes(x=reorder(Region,total_deaths),y=total_deaths)+geom_bar(stat="identity",show.legend = FALSE)
gr1+geom_text(aes(label = total_deaths,
                  angle = 0,vjust = .1))+xlab("") +
  ylab("Value") +
  theme(axis.text.x = element_text(
    hjust = 1,
    vjust = 0.5))+theme_solarized()
gr1+geom_text(aes(label = total_deaths,
                  angle = 0,vjust = .1))+xlab("") +
  ylab("Value") +
  theme(axis.text.x = element_text(
    hjust = 1,
    vjust = 0.5))+theme_void()
gr1+geom_text(aes(label = total_deaths,
                  angle = 0,vjust = .1))+xlab("") +
  ylab("Value") +
  theme(axis.text.x = element_text(
    hjust = 1,
    vjust = 0.5))+theme_solarized()
gr1+geom_text(aes(label = total_deaths,
                  angle = 0,vjust = .1))+xlab("") +
  ylab("Value") +
  theme(axis.text.x = element_text(
    hjust = 1,
    vjust = 0.5))+theme_solarized()+coord_flip()+
  theme(axis.text.y = element_text(size = 14, hjust = 1, family = "Fira Sans"),
        plot.margin = margin(rep(15, 4)))
gr1+geom_col() +
  geom_text(
    aes(label = perc),
    hjust = 1, nudge_x = -.5,
    size = 4, fontface = "bold", family = "Fira Sans"
  ) +
  ## reduce spacing between labels and bars
  scale_x_continuous(expand = c(.01, .01)) +
  scale_fill_identity(guide = "none") +
  ## get rid of all elements except y axis labels + adjust plot margin
  theme_void() +
  theme(axis.text.y = element_text(size = 14, hjust = 1, family = "Fira Sans"),
        plot.margin = margin(rep(15, 4)))
gr1+geom_col() +
  geom_text(
    aes(label = total_deaths),
    hjust = 1, nudge_x = -.5,
    size = 4, fontface = "bold", family = "Fira Sans"
  ) +
  ## reduce spacing between labels and bars
  scale_x_continuous(expand = c(.01, .01)) +
  scale_fill_identity(guide = "none") +
  ## get rid of all elements except y axis labels + adjust plot margin
  theme_void() +
  theme(axis.text.y = element_text(size = 14, hjust = 1, family = "Fira Sans"),
        plot.margin = margin(rep(15, 4)))
gr1+geom_col() +
  geom_text(
    aes(label = total_deaths),
    hjust = 1, nudge_x = -.5,
    size = 4, fontface = "bold", family = "Fira Sans"
  ) +
  ## get rid of all elements except y axis labels + adjust plot margin
  theme_void() +
  theme(axis.text.y = element_text(size = 14, hjust = 1, family = "Fira Sans"),
        plot.margin = margin(rep(15, 4)))
gr1+geom_col() +
  geom_text(
    aes(label = total_deaths),
    hjust = 1, nudge_x = -.5,
    size = 4, fontface = "bold", family = "Fira Sans"
  ) +
  ## get rid of all elements except y axis labels + adjust plot margin
  theme_void() +
  theme(axis.text.y = element_text(size = 14, hjust = 1, family = "Fira Sans"),
        plot.margin = margin(rep(15, 4)))+coord_flip()
gr1+
  geom_col(fill = "gray70") +
  geom_text(
    aes(label = total_deaths),
    ## make labels left-aligned
    hjust = 1, nudge_x = -.5
  ) +
  theme_minimal()
gr1+
  geom_col(fill = "gray70") +
  geom_text(
    aes(label = total_deaths),
    ## make labels left-aligned
    hjust = 1, nudge_x = -.5
  ) +
  theme_minimal()+coord_flip()
gr1+
  geom_col(fill = "gray70") +
  geom_text(
    aes(label = total_deaths),
    ## make labels left-aligned
    hjust = 1, nudge_x = -.5
  ) +
  theme_minimal()+coord_flip()
## create color palette based on input data
pal <- c(
  "gray85",
  rep("gray70", length(mpg_sum$manufacturer) - 4),
  "coral2", "mediumpurple1", "goldenrod1"
)
gr1+
  geom_col(fill = "gray70") +
  geom_text(
    aes(label = total_deaths),
    ## make labels left-aligned
    hjust = 1, nudge_x = -.5
  ) +
  theme_minimal()+coord_flip()+geom_col() +
  geom_text(
    aes(label = perc),
    hjust = 1, nudge_x = -.5
  ) +
  ## add custom colors
  scale_fill_manual(values = pal, guide = "none") +
  theme_minimal()
gr1+
  geom_col(fill = "gray70") +
  geom_text(
    aes(label = total_deaths),
    ## make labels left-aligned
    hjust = 1, nudge_x = -.5
  ) +
  theme_minimal()+coord_flip()+geom_col()
gr1+
  geom_col(fill = "gray70") +
  geom_text(
    aes(label = total_deaths),
    ## make labels left-aligned
    hjust = 1, nudge_x = -.5
  ) +
  theme_minimal()+coord_flip()
gr1+
  geom_col(fill = "gray70") +
  geom_text(
    aes(label = total_deaths),
    ## make labels left-aligned
    hjust = 1, nudge_x = -1
  ) +
  theme_minimal()+coord_flip()
gr1+
  geom_col(fill = "gray70") +
  geom_text(
    aes(label = total_deaths),
    ## make labels left-aligned
    hjust = 1
  ) +
  theme_minimal()+coord_flip()
gr1+
  geom_col(fill = "gray70") +
  geom_text(
    aes(label = total_deaths),
    ## make labels left-aligned
    hjust = 1
  ) +
  theme_void()+coord_flip()
minimal
gr1+
  geom_col(fill = "gray70") +
  geom_text(
    aes(label = total_deaths),
    ## make labels left-aligned
    hjust = 1
  ) +
  theme_minimal()+coord_flip()
gr1+
  geom_col(fill = "gray70") +
  geom_text(
    aes(label = total_deaths),
    ## make labels left-aligned
    hjust = 1
  ) +
  theme_minimal()+coord_flip()+labs(title = "Number of deaths due to floods from 14-Jun to 27-Aug,2022")
gr1+
  geom_col(fill = "gray70") +
  geom_text(
    aes(label = total_deaths),
    ## make labels left-aligned
    hjust = 1
  ) +
  theme_minimal()+coord_flip()+labs(x="",y="",title = "Number of deaths due to floods from 14-Jun to 27-Aug,2022")
gr1+
  geom_col(fill = "gray70") +
  geom_text(
    aes(label = total_deaths),
    ## make labels left-aligned
    hjust = 1
  ) +
  theme_wsj()+coord_flip()+labs(x="",y="",title = "Number of deaths due to floods from 14-Jun to 27-Aug,2022")
gr1_final<-gr1+
  geom_col(fill = "gray70") +
  geom_text(
    aes(label = total_deaths),
    ## make labels left-aligned
    hjust = 1
  ) +
  theme_minimal()+coord_flip()+labs(x="",y="",
                                    title = "Number of deaths due to floods from 14-Jun to 27-Aug,2022",
                                    caption="by Zahid ,source:NDMA")

gr1+
  geom_col(fill = "gray70") +
  geom_text(
    aes(label = total_deaths),
    ## make labels left-aligned
    hjust = 1
  ) +
  theme_minimal()+coord_flip()+labs(x="",y="",title = "Number of deaths due to floods from 14-Jun to 27-Aug,2022")
gr2<-df_flood %>% filter(Category %in% c("M_injured","F_injured", "C_injured")) %>% group_by(Region) %>% summarize(total_injured=sum(Number)) %>%
  ggplot()+aes(x=reorder(Region,total_injured),y=total_injured)+geom_bar(stat="identity",show.legend = FALSE)
gr2_final<-gr2+geom_col(fill = "gray70") +
  geom_text(
    aes(label = total_injured),
    ## make labels left-aligned
    hjust = 1
  ) +
  theme_minimal()+coord_flip()+labs(x="",y="",
                                    title = "Number of injured persons due to floods from 14-Jun to 27-Aug,2022",
                                    caption="by Zahid, data from NDMA")
gr3<-df_flood %>% filter(Category %in% c("Livestock")) %>% group_by(Region) %>% summarize(livestock=sum(Number)) %>%
  ggplot()+aes(x=reorder(Region,livestock),y=livestock)+geom_bar(stat="identity",show.legend = FALSE)
gr3<-df_flood %>% filter(Category %in% c("Livestock")) %>% group_by(Region) %>% summarize(livestock=sum(Number)) %>%
  ggplot()+aes(x=reorder(Region,livestock),y=livestock)+geom_bar(stat="identity",show.legend = FALSE)
gr3+geom_col(fill = "gray70") +
  geom_text(
    aes(label = livestock),
    ## make labels left-aligned
    hjust = 1
  ) +
  theme_minimal()+coord_flip()+labs(x="",y="",title = "Livestocks loss due to floods from 14-Jun to 27-Aug,2022")
gr3+geom_col(fill = "gray70") +
  geom_text(
    aes(label = livestock),
    ## make labels left-aligned
    hjust = 0.5
  ) +
  theme_minimal()+coord_flip()+labs(x="",y="",title = "Livestocks loss due to floods from 14-Jun to 27-Aug,2022")
scipen(999)
scipn(999)
sciepen(999)
options(scipen = 999)
gr3<-df_flood %>% filter(Category %in% c("Livestock")) %>% group_by(Region) %>% summarize(livestock=sum(Number)) %>%
  ggplot()+aes(x=reorder(Region,livestock),y=livestock)+geom_bar(stat="identity",show.legend = FALSE)
gr3_final<-gr3+geom_col(fill = "gray70") +
  geom_text(
    aes(label = livestock),
    ## make labels left-aligned
    hjust = 0.5
  ) +
  theme_minimal()+coord_flip()+labs(x="",y="",
                                    title = "Livestocks loss due to floods from 14-Jun to 27-Aug,2022",
                                    caption = "by Zahid, data from NDMA")
library(gridExtra)
grid.arrange(gr1_final,gr2_final,gr3_final)
gr1_final
gr2_final
gr3_final
