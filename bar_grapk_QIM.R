library(tidyverse)
library(ggplot2)
library(patchwork)
library(tidyr)
data("mtcars")
# x-axis as continuous
p1 <- ggplot(mtcars, aes(x = cyl)) +
  geom_bar() +
  ggtitle("Panel A: x-axis as a continuous variable")

# x-axis as categorical
p2 <- ggplot(mtcars, aes(x = factor(cyl))) +
  geom_bar() +
  ggtitle("Panel B: x-axis as a categorical (factor) variable")

p1+p2

mtcars |> summarise(avg_mpg=mean(mpg),na.rm=TRUE,.by=cyl) |> 
  ggplot()+aes(x=factor(cyl),y=avg_mpg)+geom_bar(stat="identity")



p1 <- ggplot(mtcars, aes(x = factor(cyl))) +
  geom_bar(width = .5) +
  ggtitle("bar width = 0.5")


p2 <- ggplot(mtcars, aes(x = factor(cyl))) +
  geom_bar(width = .75) +
  ggtitle("bar width = 0.75")

p3 <- ggplot(mtcars, aes(x = factor(cyl))) +
  geom_bar(width = .9) +
  ggtitle("bar width = 0.9")

p4 <- ggplot(mtcars, aes(x = factor(cyl))) +
  geom_bar(width = .99) +
  ggtitle("bar width = 0.99")
p1+p2+p3+p4


library(tidyverse)

df <- tribble(
  ~Sector,	~LSM,
  "Manufacturing",	-8.1,
  "Electricity generation",	-10,
  "Petrol sales",	-17,
  "Diesel",	-28,
  "Cement",	-18
  
)
library(forcats)
library(ggthemes)
df |> mutate(Sector = fct_reorder(Sector, LSM)) |> 
ggplot() + aes(x = Sector, y = LSM, fill = Sector) + geom_bar(stat = "identity") +
  coord_flip() +
  theme(legend.position = "none") +
  geom_text(aes(label=LSM), hjust=-0.5)+
  labs(x=element_blank(),y=element_blank())+
  labs(title = "Economy on nosedive first 9-10 months",
       subtitle = "Low consumption of Diesel, Petrol, Cement and low generation of electricity indicating economy \n is almost on nosedive and directly and indirectly limiting opportunities for millions of workers \n to earn their livelihood")+
  labs(caption = "brecorder, charty by Zahid")
            