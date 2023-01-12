library(readr)
library(tidyverse)
movies_ex <- read_csv("https://moderndive.com/data/movies.csv") %>%
  filter(type %in% c("action", "comedy", "drama", "animated", "fantasy", "rom comedy")) %>%
  select(-over200)

movies_ex %>%
  slice(1:10)

## Dealing with missing values
movies_ex %>%
  summarize(mean_profit = median(millions))

### you should always at the very least report to the reader if you do so, as by removing the missing values 
# you may be biasing your results.

##You can do this with a na.rm = TRUE argument like so:
  
movies_ex %>%
  summarize(mean_profit = median(millions, na.rm = TRUE))


## If you decide you want to remove the row with the missing data, you can use the filter function like so:


movies_no_missing <- movies_ex %>%
  filter(!is.na(millions))

movies_no_missing %>%
  slice(1:10)

## Let’s compute the total revenue for each movie type and plot a barplot.


revenue_by_type <- movies_ex %>%
  group_by(type) %>%
  summarize(total_revenue = sum(millions))
revenue_by_type


ggplot(revenue_by_type, aes(x = type, y = total_revenue)) +
  geom_col() +
  labs(x = "Movie genre", y = "Total box office revenue (in millions of $)")


## Say we want to reorder the categorical variable type so that the bars show in a different order. 
## We can reorder the bars by manually defining the order of the levels in the factor() command:


type_levels <- c("rom comedy", "action", "drama", "animated", "comedy", "fantasy")

revenue_by_type <- revenue_by_type %>%
  mutate(type = factor(type, levels = type_levels))

ggplot(revenue_by_type, aes(x = type, y = total_revenue)) +
  geom_col() +
  labs(x = "Movie genre", y = "Total boxoffice revenue (in millions of $)")

## Or if you want to reorder type in ascending order of total_revenue, we use reorder()


revenue_by_type <- revenue_by_type %>%
  mutate(type = reorder(type, total_revenue))

ggplot(revenue_by_type, aes(x = type, y = total_revenue)) +
  geom_col() +
  labs(
    x = "Movie genre", y = "Total boxoffice revenue (in millions of $)"
  )


## Or if you want to reorder type in descending order of total_revenue, 
## just put a - sign in front of -total_revenue in reorder():


revenue_by_type <- revenue_by_type %>%
  mutate(type = reorder(type, -total_revenue))

ggplot(revenue_by_type, aes(x = type, y = total_revenue)) +
  geom_col() +
  labs(
    x = "Movie genre", y = "Total boxoffice revenue (in millions of $)"
  )

## Showing money on an axis

movies_ex <- movies_ex %>%
  mutate(revenue = millions * 10^6)

ggplot(data = movies_ex, aes(x = rating, y = revenue)) +
  geom_boxplot() +
  labs(x = "rating", y = "Revenue in $", title = "Profits for different movie ratings")

## Google “ggplot2 axis scale dollars” and click on the first link and search for the word “dollars”. You’ll find:


# Don't forget to load the scales package first!
library(scales)

ggplot(data = movies_ex, aes(x = rating, y = revenue)) +
  geom_boxplot() +
  labs(x = "rating", y = "Revenue in $", title = "Profits for different movie ratings") +
  scale_y_continuous(labels = dollar)


## Changing values inside cells

# `if_else()`   , `recode()`, `case_when()` 

movies_ex %>%
  mutate(type_new = if_else(type == "rom comedy", "romantic comedy", type)) %>%
  slice(1:10)

movies_ex %>%
  mutate(type = if_else(type == "rom comedy", "romantic comedy", "not romantic comedy")) %>%
  slice(1:10)

# recode()

movies_ex %>%
  mutate(type_new = recode(type,
                           "action" = "Action",
                           "animated" = "Animated",
                           "comedy" = "Comedy",
                           "drama" = "Drama",
                           "fantasy" = "Fantasy",
                           "rom comedy" = "Romantic Comedy"
  )) %>%
  slice(1:10)

# case_when()
#case_when() is a little trickier, but allows you to evaluate boolean operations using ==, >, >=, &, |, etc:
  
  
  movies_ex %>%
  mutate(
    type_new =
      case_when(
        type == "action" & millions > 40 ~ "Big budget action",
        type == "rom comedy" & millions < 40 ~ "Small budget romcom",
        # Need this for everything else that aren't the two cases above:
        TRUE ~ "Rest"
      )
  )

  movies_ex %>%
    mutate(big_budget = millions > 100) %>%
    slice(1:10)  


  movies_ex %>%
    mutate(score_categ = cut(score,
                             breaks = c(0, 40, 60, 80, 100),
                             labels = c("bad", "so-so", "good", "great")
    )) %>%
    slice(1:10)  

##  Computing proportions
##  By using a group_by() followed not by a summarize() as is often the case, but rather a mutate(). So say we compute the total revenue millions for each movie rating and type:
    
rating_by_type_millions <- movies_ex %>%
    group_by(rating, type) %>%
    summarize(millions = sum(millions)) %>%
    arrange(rating, type)
  
rating_by_type_millions  


rating_by_type_millions %>%
  group_by(rating) %>%
  mutate(
    # Compute a new column of the sum of millions split by rating:
    total_millions = sum(millions),
    # Compute the proportion within each rating:
    prop = millions / total_millions
  )

#  Dealing with %, commas, and $

library(readr)
parse_number("10.5%")

parse_number("145,897")

parse_number("$1,234.5")

parse_number("$1,234.5")

comma(145897)
dollar(1234.5)
