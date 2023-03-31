#install.packages("gapminder")
#install.packages("tidyverse")
library(gapminder)
library(tidyverse)

# look at data
diamonds

# another useful command
glimpse(diamonds)


as_tibble(diamonds)

## Arrange
# order by year, with the smallest to largest pop in each year
gapminder |> 
  arrange(year, pop)


## Arrange by Descending order
gapminder |>
  arrange(desc(pop))

## Select

gapminder |>
  select(country, year, pop)

# keep all variables *except* gdpPercap
gapminder |>
  select(-gdpPercap)

# Reordering column

# move pop to first column
gapminder |>
  select(pop, everything())

# keep all variables starting with "co"
gapminder |>
  select(starts_with("co"))

# keep country and all variables containing "per"
gapminder |>
  select(country, contains("per"))
gapminder
# rename gdpPercap to GDP and lifeExp to population
gapminder |>
  rename(GDP = gdpPercap,
         LE = lifeExp)

## We are going to filter data for Pakistan and India and comparing
#

#install.packages("tidyrgee")
library(tidyrgee)
library(help=tidyrgee)

## Filter
# look only at African observations
gapminder |>
  filter(country %in% c("Pakistan","India", "Sri Lanka", "Bangladesh"), year==2007)

# look only at African observations in 1997
gapminder |>
  filter(continent == "Africa",
         year == 1997)



## `filter()` with Conditionals II


# look only at African observations OR observations in 1997
gapminder |>
  filter(continent == "Africa" | 
           year == 1997)

## `filter()` with Conditionals III
# look only at U.S. and U.K. observations in 2002
gapminder |>
  filter(country %in% 
           c("United States",
             "United Kingdom"),
         year == 2002)

## Mutate
# create variable called "europe" if country is in Europe
mutate(gapminder, 
       europe = case_when(continent == "Europe" ~ "In Europe",
                          continent != "Europe" ~ "Not in Europe"))



## `mutate()`: Changing a Variable's Scale

# create population in millions variable
gapminder |>
  mutate(pop_mil = pop / 1000000)

## `mutate()`: Variable Based on Other Variables


# create GDP variable from gdpPercap and pop, in billions
gapminder |>
  mutate(GDP = ((gdpPercap * pop) / 1000000000))

## `mutate()`: Change Class of Variable


# change year variable from an integer to a factor
gapminder |>
  mutate(year = as.factor(year))

## `mutate()`: Create Multiple Variables


gapminder |>
  mutate(GDP = gdpPercap * pop,
         pop_millions = pop / 1000000)



## `transmute()`: Keep Only New Variables


gapminder |>
  transmute(GDP = gdpPercap * pop,
            pop_millions = pop / 1000000)


## `mutate()`: Conditionals

#   Boolean, logical, and conditionals all work well in `mutate()`:

gapminder |>
  select(country, year, lifeExp) |>
  mutate(long_life_1 = lifeExp > 70,
         long_life_2 = case_when(lifeExp > 70 ~ "Long",
                                 lifeExp <= 70 ~ "Short"))


## `mutate()` is Order Aware


gapminder |>
  select(country, year, lifeExp) |>
  mutate(dog_years = lifeExp * 7,
         comment = paste("Life expectancy in", country, "is", dog_years, "in dog years.", sep = " "))

## `mutate()`: Scoped-functions I

#   "Scoped" variants of `mutate` that work on a subset of variables:
#  -   `mutate_all()` affects every variable
#-   `mutate_at()` affects named or selected variables
#-   `mutate_if()` affects variables that meet a criteria
# round all observations of numeric variables to 2 digits
gapminder |>
  mutate_if(is.numeric, round, digits = 2)

## `mutate()`: Scoped-functions II

#-   "Scoped" variants of `mutate` that work on a subset of variables:
# -   `mutate_all()` affects every variable
#   `mutate_at()` affects named or selected variables
#   `mutate_if()` affects variables that meet a criteria
# make all factor variables uppercase
gapminder |>
  mutate_if(is.factor, toupper)

## A Reminder on Viewing, Saving, & Overwriting Objects I

#-   `dplyr` functions never modify their inputs (i.e. never overwrite the original `tibble`)
#-   If you want to save a result, use `<-` to assign it to a new `tibble`
#-   If assigned, you will not see the output until you call up the new `tibble` by name

# Prints output, doesn't save/overwrite object
gapminder |>
  filter(continent == "Africa") 

# Saves as africa
africa <- gapminder |>
  filter(continent == "Africa")

# Look at it
africa

## A Reminder on Viewing, Saving, & Overwriting Objects II

#-   Neat trick:
# Save and view at same time by wrapping whole command with ()
(africa <- gapminder |>
    filter(continent == "Africa"))



## `summarize()`

# get average life expectancy and call it avg_LE

gapminder |>
  summarize(avg_LE = mean(lifeExp))

## `summarize()`: Useful commands

-   Useful `summarize()` commands:
  
  | Command        | Does                                                         |
  |-------------------------------------------|-----------------------------|
  | `n()`          | Number of observations                                       |
  | `n_distinct()` | Number of unique observations                                |
  | `sum()`        | Sum all observations of a variable                           |
  | `mean()`       | Average of all observations of a variable                    |
  | `median()`     | 50<sup>th</sup> percentile of all observations of a variable |
  | `sd()`         | Standard deviation of all observations of a variable         |
  
  
|Indicator    | Value|
|-------------|------|
|Policy Rate  |16%   |	
|Inflation     | 	24%|
|Foreign Reserves|	Less than 8 USD (one months import while 3 months are considered comfortable level)|
|Exchange Rate	|Highly volatile|
|Political stability (2000 onward)|	Bottom 10 countries|

  

  
  