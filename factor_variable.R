library(tidyverse)
library(gapminder)

## Factor Inspection
gapminder|>glimpse()
levels(gapminder$continent)
nlevels(gapminder$continent)
class(gapminder$continent)

## To get a frequency table
gapminder|>count(continent)

## Dropping unused levels
nlevels(gapminder$country)
h_countries<-c("India", "Bangladesh", "Pakistan", "Sri Lanka", "Afghanistan")
h_gap<-gapminder|>
  filter(country %in% h_countries)
nlevels(h_gap$country)
## Despite data only for 5 countries why we have 142 levels
## and how can we drop 
h_gap_dropped<- h_gap|>
  droplevels()
nlevels(h_gap_dropped$country)

# use forcats:: fct_drop() on a free-range factor
h_gap$country |>
  fct_drop() |>
  levels()

## Exercise
pop_25<-gapminder|>filter(pop<250000)
nlevels(pop_25$country)
pop_25$country|>
  fct_drop()|>
  levels()
pop_25_drop<-pop_25|>droplevels()
nlevels(pop_25_drop$country)



## default order is alphabetical 
gapminder$continent|>
  levels()

## order by frequency
gapminder$continent|>fct_infreq()|>
  levels()

## backward
gapminder$continent|>fct_infreq()|>
  fct_rev()|>
  levels()


## Order country by medial life expectancy
fct_reorder(gapminder$country,gapminder$lifeExp)|>
  levels()|>head()

## order according to minimum life expectancy instead of median
fct_reorder(gapminder$country,gapminder$lifeExp,min)|>
  levels()|>head()

## backwards!
fct_reorder(gapminder$country,gapminder$lifeExp,.desc = TRUE )|>
  levels()|>head()




gap_asia_2007 <- gapminder %>% filter(year == 2007, continent == "Asia")
ggplot(gap_asia_2007, aes(x = lifeExp, y = country)) + geom_point()
ggplot(gap_asia_2007, aes(x = lifeExp, y = fct_reorder(country, lifeExp))) +
  geom_point()


## fct_reorder2 for line plot
h_gap <- gapminder %>%
  filter(country %in% h_countries) %>% 
  droplevels()
ggplot(h_gap, aes(x = year, y = lifeExp, color = country)) +
  geom_line()
ggplot(h_gap, aes(x = year, y = lifeExp,
                  color = fct_reorder2(country, year, lifeExp))) +
  geom_line() +
  labs(color = "country")

## Change order of the levels because I said so

h_gap$country|> levels()

h_gap$country|>fct_relevel("Afghanistan","Bangladesh","India",
                           "Pakistan", "Sri Lanka")


## Recode the levels
i_gap<-gapminder|>
  filter(country %in% c("Pakistan", "Sri Lanka","Bangladesh","India",
                        ))|>
  droplevels()
i_gap$country|>levels()

i_gap$country|>
  fct_recode("Pak"="Pakistan", "SL"="Sri Lanka",
             "Ind"="India", "BD"="Bangladesh")|>
  levels()
