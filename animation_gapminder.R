library(tidyverse)
library(gapminder)

library(gganimate)


gapminder %>% filter(country%in% c("India", "Bangladesh", "Pakistan")) %>% 
  ggplot(data=.)+aes(x=year, y=gdpPercap, color=country)+geom_line()+
  transition_reveal(year)




gapminder %>%
  filter(country == "Pakistan" |
           country == "India" | country == "Bangladesh") %>%
  ggplot(aes(year, pop, color = country)) + geom_point() + geom_line() + 
  scale_y_log10() +
  geom_text(
    aes(
      x = min(year),
      y = min(pop),
      label = as.factor(year)
    ) ,
    hjust = -2,
    vjust = -0.2,
    alpha = 0.5,
    col = "gray",
    size = 20
  ) +
  theme_minimal() +
  transition_reveal(year) +
  view_follow()
anim_save("gapminder.gif")
library(gapminder)
library(tidyverse)
library(gganimate)
gapminder$gdpPercap<-round(gapminder$gdpPercap,0)
datos2 <- gapminder %>%
  group_by(year) %>%
  arrange(year, desc(round(gdpPercap,0))) %>%
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