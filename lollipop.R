library(tidyverse)
library(readxl)
library(janitor)
library(ggthemes)
out_of_sch<- read_excel("data/out_of_school.xlsx")

out_of_sch<-out_of_sch |> clean_names()

out_of_sch |> 
  ggplot()+aes(x=reorder(district,oosc),y=oosc)+geom_point(size=3,colour="maroon")+
  geom_segment(aes(x=district,y=0,xend=district,yend=oosc),size=0.6,colour="blue")+
  coord_flip()+theme_tufte()+
  labs(y="Out of School Children",x=" ")+
  labs(title = "22% of children aged 5-16 are out of school in Punjab",
       subtitle = "Southern Punjab significant proportion school going children are out of school", 
       caption = "GoP, Zahid Asghar")






















manufacturers <- mpg |> 
  count(manufacturer, sort = TRUE) |> 
  mutate(
    manufacturer = str_to_title(manufacturer),
    manufacturer = fct_reorder(manufacturer, n) 
  )
manufacturers
## Bar plot
manufacturers |> 
  ggplot(aes(y = manufacturer, x = n)) +
  geom_col(fill = 'dodgerblue4') +
  theme_minimal() +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = element_blank(), 
    y = element_blank(),
    title = 'Number of cars in the {mpg} data set'
  ) +
  theme(
    panel.grid.major.y = element_blank()
  )

## Lollipop chart
manufacturers |> 
  ggplot(aes(y = manufacturer, x = n)) +
  geom_point(col = 'dodgerblue4', size = 5) +
  geom_segment(
    aes(x = 0, xend = n, y = manufacturer, yend = manufacturer),
    linewidth = 1.5,
    col = 'dodgerblue4'
  ) +
  theme_minimal() +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = element_blank(), 
    y = element_blank(),
    title = 'Number of cars in the {mpg} data set'
  ) +
  theme(
    panel.grid.major.y = element_blank()
  )
