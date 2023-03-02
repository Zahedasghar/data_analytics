library(tidyverse)
library(gapminder)



gapminder |>
  select(country, lifeExp, year) ->
  country_life_exp

gapminder |>
  select(country, pop, year) ->
  country_pop

country_pop |>
  full_join(country_life_exp, by = c("country", "year"))
