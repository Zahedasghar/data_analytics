

library(haven)
library(tidyverse)
library(gt)
library(gtsummary)
library(gtExtras)
library(vtable)
ahk<-read_spss("AHK-health.sav")
ahk<-as_tibble(ahk)
View(ahk)

ahk %>% glimpse()

ahk_other<-ahk %>% select(contains("_other"))

ahk_other %>% glimpse()

vtable(ahk)

ahk %>% select(S2Q2, S2Q6,S2Q26)

ahk %>% mutate_at(1:416, factor)

## How to select variables which are not others
ahk %>% select(-contains("_other"))
View(ahk)

#ahk_sample<-ahk %>% sample_n(300) %>% select(starts_with("S2"))
#saveRDS(ahk_sample, file="ahk_small.rds")
#ahks<-readRDS("ahk_small.rds")
#ahk %>% select(contains("_other"))
#ahks %>% select(starts_with("S2"))
#View(ahks)
library(vtable)
library(modelsummary)
vtable(ahk)

## To see all data labels , variables and their scales
ahk %>% sjPlot::view_df()


glimpse(ahk)

## Select all variables containing other in it

ahk %>% select(S2Q27) %>% mutate(S2Q27=as_factor(S2Q27)) %>%  drop_na() %>%  tbl_summary()

## Summarise by mode of transportation

ahk %>% select(S2Q32) %>% tbl_summary() %>% as_gt() %>% 
  gt_theme_ft()


ahk %>% select(S2Q32) %>% tbl_summary() %>% as_gt() %>% 
gt_theme_nytimes()

ahk %>% select(S2Q32) %>% tbl_summary() %>% as_gt() %>% 
  gt_theme_guardian()




ahk %>% select(S2Q32) %>% tbl_summary() %>% as_gt() %>% 
  gt_theme_guardian() %>% 
  tab_header(title = "Table 1: Mode of transportation to be used")

ahk %>% select(S2Q32) %>% tbl_summary() %>% as_gt() %>% 
  gt_theme_guardian() %>% 
  tab_header(title = "Table 1: Mode of transportation to be used") %>% 
  row_spec(c(3, 5), background = "#F5ABEA")


ahk %>% 
  dplyr::select(S2Q32) %>% drop_na() %>% 
  # convert labelled values to a factor ----
mutate_at(vars(matches("S")), haven::as_factor) %>% 
  gtsummary::tbl_summary() %>% as_gt() %>% 
  gt_theme_guardian() %>% 
  tab_header(title = "Table 1: Mode of transportation to be used")

## Missing no and yes
ahk %>% 
  dplyr::select(S2Q32) %>% 
  # convert labelled values to a factor ----
mutate_at(vars(matches("S")), haven::as_factor) %>% 
  gtsummary::tbl_summary(missing = "no") %>% as_gt() %>% 
  gt_theme_guardian() %>% 
  tab_header(title = "Table 1: Mode of transportation to be used")

ahk %>% 
  dplyr::select(S2Q32) %>% 
  # convert labelled values to a factor ----
mutate_at(vars(matches("S")), haven::as_factor) %>% 
  gtsummary::tbl_summary() %>% as_gt() %>% 
  gt_theme_guardian() %>% 
  tab_header(title = "Table 1: Mode of transportation to be used") 


library(patchwork)


ahk %>% 
  dplyr::select(S2Q32_other) %>% 
  # convert labelled values to a factor ----
mutate_at(vars(matches("S")), haven::as_factor) %>% 
  gtsummary::tbl_summary() %>% as_gt() %>% 
  gt_theme_guardian() %>% 
  tab_header(title = "Table 1: Mode of transportation to be used")


ahk %>% select(S3Q1) %>% tbl_summary()

ahk %>% select(S2Q31) %>% mutate(S2Q31=as.factor(S2Q31)) %>% tbl_summary() ## This should be a factor variable


ahk %>% select(S3Q16_1) %>% tbl_summary() %>% as_gt() %>% 
  gt_theme_guardian()

ahk %>% select(S6Q26) %>% tbl_summary()

ahk %>% select(S2Q15,S6Q26) %>% group_by(S2Q15) %>% tbl_summary()


ahk %>% tbl_cross(
  row=S2Q15,
  col=S6Q26,
  percent = "cell"
) %>% add_p()



ahk %>% tbl_cross(
  row = S2Q15,
  col = S6Q26,
  percent = "cell"
) %>%
  add_p()

ahk %>% tbl_cross(
  row = S2Q7,
  col = S6Q26,
  percent = "cell"
) %>%
  add_p()


ahk %>% tbl_cross(
  row = S2Q22,
  col = S6Q26,
  percent = "cell"
) %>%
  add_p()

ahk %>% sample_n(200)

summary(ahk$S6Q26)

ahk %>% select(S6Q26, S3Q19_10) %>% tbl_summary()
ahk %>% glimpse()
## Ethnicity

ahk %>% select(S2Q2_other) %>% tbl_summary()






ahk %>% select(S2Q2) %>% tbl_summary()

ahk %>% select(S2Q2_other) %>% tbl_summary()


















ahk %>% select(S2Q2) %>% mutate(S2Q2=as_factor(S2Q2)) %>% tbl_summary()
## Simply replace S2Q2 98 with 9

ahk<-ahk %>% mutate(S2Q2=replace(S2Q2,S2Q2==98,9)) 


ahk$S2Q2_1<-ifelse(is.na(ahk$S2Q2_other),ahk$S2Q2,ahk$S2Q2_other)



ahk<-ahk %>% mutate(S2Q2_1=recode(S2Q2_1, "Hindko"="9", "Hazara wal"="9"))



ahk<-ahk %>% mutate(

ahk %>% select(S2Q2) %>% mutate(S2Q2=as_factor(S2Q2)) %>% tbl_summary()
mutate(x1 = replace(x1, x1 == 2, 99))


 ahk %>% select(S2Q2_1) %>% tbl_summary()
saveRDS(ahk, file = "AHK.rds")
AHK<-readRDS("AHK.rds")
AHK

#save(ahk, file = "AHK1.RData")
#AHK1<-load("AHK1.RData")
#AHK1

AHK_short<-AHK %>% select(S2Q2, S2Q2_other) 
saveRDS(AHK_short, file = "AHK1.rds")


View(AHK_short)




library(gapminder)

library(gganimate)

gapminder %>% filter(country%in% c("India", "Bangladesh", "Pakistan")) %>% 
  ggplot(data=.)+aes(x=year, y=pop, color=country)+geom_line()+
  transition_reveal(year)




gapminder %>%
  filter(country == "Pakistan"|country == "India"|country == "Bangladesh") %>%
  ggplot(aes(year, pop, color=country)) + geom_point() + geom_line() + 
  geom_text(aes(x = min(year), y = min(pop), label = as.factor(year)) , hjust=-2, vjust = -0.2, alpha = 0.5,  col = "gray", size = 20) +
  theme_minimal() +
  transition_reveal(year) + 
  view_follow()
anim_save("gapminder.gif")
library(gapminder)
library(tidyverse)
library(gganimate)
datos2 <- gapminder %>%
  group_by(year) %>%
  arrange(year, desc(gdpPercap)) %>%
  mutate(ranking = row_number()) %>%
  filter(country %in% c("Pakistan", "India","Bangladesh", "Sri Lanka"))




animacion <- datos2 %>%
  ggplot() +
  geom_col(aes(ranking, gdpPercap, fill = country)) +
  geom_text(aes(ranking, gdpPercap, label = gdpPercap), hjust=-0.1) +
  geom_text(aes(ranking, y=0 , label = country), hjust=1.1) + 
  geom_text(aes(x=15, y=max(gdpPercap) , label = as.factor(year)), vjust = 0.2, alpha = 0.5,  col = "gray", size = 20) +
  coord_flip(clip = "off", expand = FALSE) + scale_x_reverse() +
  theme_minimal() + theme(
    panel.grid = element_blank(), 
    legend.position = "none",
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    plot.margin = margin(1, 4, 1, 3, "cm")
  ) +
  transition_states(year, state_length = 0, transition_length = 2) +
  enter_fade() +
  exit_fade() + 
  ease_aes('quadratic-in-out') 

animate(animacion, width = 700, height = 432, fps = 25, duration = 15, rewind = FALSE)
Bar chart race animation created in R with gganimate
Bar chart race animation created in R with gganimate
Conclusion on how to create animations in R: taking the graphs to the next level
Knowing how to create animation in R is something very easy, but very practical too. If you want to create graphs that have a higher impact and you have the chance of showing that graph as an animation, I would recommend you to do so. But animations are more useful than that.

In my case, for example, I have used the animations to explain in a super simple and visual way how the K-means algorithm works (link). This could also be used on how neural networks work or how a neural network performance improves with more learning. We could even use animations on sports analytics!
  
  In summary, whatever your work is, I hope that you have found interesting learning how to create animations in R with gganimate.

As always, if you would like me to write about a specific topic, do not hesitate to reach out on Linkedin. See you at the next one!