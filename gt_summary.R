library(tidyverse)
library(gt)
gapminder_data <- gapminder::gapminder |> 
  janitor::clean_names() |> 
  select(continent, country, year, life_exp) |> 
  mutate(
    year = as.character(year),
    # Year is really categorical with numeric labels
    country = as.character(country) 
  ) 
gapminder_data
library(gtExtras)
gt_plt_summary(gapminder_data) 

selected_countries <- gapminder_data  |> 
  # Filter to use only six years (those that end in 7)
  filter(str_ends(year, "7")) |>
  # sample two countries per continent
  group_by(continent, country) |> 
  nest() |> 
  group_by(continent) |> 
  slice_sample(n = 2) |> 
  ungroup() |> 
  unnest(data) |> 
  # Rearrange the data into table format
  pivot_wider(names_from = year, names_prefix = 'year', values_from = life_exp)
selected_countries


# New column names
new_colnames <- colnames(selected_countries) |> str_remove('(country|year)')
names(new_colnames) <- colnames(selected_countries)

selected_countries |> 
  gt(groupname_col = 'continent') |> 
  tab_header(
    title = 'Life Expectancies over time',
    subtitle = 'Data is courtesy of the Gapminder foundation'
  ) |> 
  cols_label(.list = new_colnames) |> 
  fmt_number(columns = where(is.numeric), decimals = 2) |> 
  gt_theme_538()




# Two colors from the Okabe Ito color palette
color_palette <- c("#CC79A7", "#009E73")

selected_countries |> 
  gt(groupname_col = 'continent') |> 
  tab_header(
    title = 'Life Expectancies over time',
    subtitle = 'Data is courtesy of the Gapminder foundation'
  ) |> 
  cols_label(.list = new_colnames) |> 
  fmt_number(columns = where(is.numeric), decimals = 2) |> 
  gt_theme_538() |> 
  gt_color_rows(
    columns = year2007, 
    domain = c(30, 85),
    palette = color_palette
  )

# Two colors from the Okabe Ito color palette
color_palette <- c("#CC79A7", "#009E73")

selected_countries |> 
  gt(groupname_col = 'continent') |> 
  tab_header(
    title = 'Life Expectancies over time',
    subtitle = 'Data is courtesy of the Gapminder foundation'
  ) |> 
  cols_label(.list = new_colnames) |> 
  fmt_number(columns = where(is.numeric), decimals = 2) |> 
  gt_theme_538() |> 
  gt_color_rows(
    columns = c(year1957, year2007), 
    domain = c(30, 85),
    palette = color_palette
  )
