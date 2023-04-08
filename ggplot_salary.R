library(tidyverse)
library(car)
library(forcats)
theme_set(theme_bw())
head(Salaries)


sdf <- Salaries

sdf |> glimpse()

colnames(sdf)

library(forcats)
library(gt)
library(gtsummary)
library(modelsummary)
library(gtExtras)

sdf |> select(is.factor) |> tbl_summary()

sdf |> select(is.factor) |> tbl_summary()

## 9 months salary histogram
sdf %>% ggplot() +
  geom_histogram(aes(x = salary), color = "black", fill = "blue", bins = 50) +
  labs(x = "Nine-Month Salary (in dollars)", y = "Count", 
       title = "Nine-Month Salary Distribution")

## Years since PhD 

sdf %>% ggplot() +
  geom_histogram(aes(x = yrs.since.phd), color = "black", fill = "green", bins = 50) +
  labs(x = "Years since Ph.D.", y = "Count", 
       title = "Years since Ph.D. Distribution")

## Years of service
sdf %>% ggplot() +
  geom_histogram(aes(x = yrs.service), color = "black", fill = "orange", bins = 50) +
  labs(x = "Years of Service", y = "Count", 
       title = "Years of Service Distribution")




Salaries |> 
  ggplot(aes(x=yrs.since.phd,y=salary,colour=rank))+
  geom_point()

# Make shape=discipline
# change geom_point to geom_jitter
# Add linear model by sex
# Facet by sex
# Add axis and legend labels


Salaries |> 
  ggplot(aes(x=yrs.since.phd,y=salary))+
  geom_jitter(aes(color=rank, shape=discipline))+
  geom_smooth(method=lm)+
  facet_wrap(~sex)+
  labs(title = "Salary vs years since PhD",
       x="Years since PhD",
       y="Income",
       color="Position",
       shape="Research area")

Salaries |> 
  filter(salary<150000) |> 
  ggplot(aes(x=rank,y=salary,fill=sex))+
  geom_boxplot(alpha=0.5)+
  labs(title = "Faculty Salary by Rank and Gender",
       x="",y="",
       fill="Gender")

Salaries |> 
  filter(salary<150000) |> 
  ggplot(aes(x=rank,y=salary,fill=sex))+
  geom_boxplot(alpha=0.5)+
  scale_x_discrete(breaks=c("AsstProf","AssocProf",
                            "Prof"),
                   labels=c("Assistant\nProfessor",
                            "Associate\nProfessor",
                            "Full\nProfessor"))+
  scale_y_continuous(breaks = c(50000,100000,150000,
                                200000),
                     labels = c("$50K","$100K",
                                "$150K","$200K"))+
    labs(title = "Faculty Salary by Rank and Gender",
       x="",y="",
       fill="Gender")+
  theme(legend.position = c(0.11,0.78))

























theme_set(theme_bw()+
            theme(title = element_text(colour = "steelblue",
                                       face = "bold")))



Salaries |> 
  mutate(discipline=factor(discipline,
                           levels=c("A","B"),
                           labels=c("Agriculture","Biology"))) |> 
  ggplot(aes(yrs.since.phd,salary))+
  geom_point(aes(color=rank),alpha=0.5)+
  geom_smooth()+
  facet_grid(sex~discipline)+
  labs(title="Salary vs years since PhD",
       x="Number of years since PhD",
       y="Current salary",
       color="Position")



