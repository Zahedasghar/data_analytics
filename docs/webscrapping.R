library(rvest)
library(tidytable)
library(xml2)
library(tidyverse)
df1 <- 
  read_html("https://www.psx.com.pk/psx/announcement/financial-announcements") %>% 
  html_elements("body") %>%
  html_table() %>%
  .[[1]]

df1


df2 <- read_html("https://dps.psx.com.pk/") |> 
  html_elements("body") |> 
  html_table() 


#
#        Author: Dr Eugene O'Loughlin
#   Video Title: How To... Read HTML Tables from the Internet in R
#  Video Number: #25
#
# Install and load necessary packages
#install.packages("rvest")
library(rvest)
#
# First Example
# URL for List of Highest Grossing Films in USA and Canada (Wikipedia)
# https://en.wikipedia.org/wiki/List_of_highest-grossing_films_in_the_United_States_and_Canada
#
filmsURL <- read_html("https://en.wikipedia.org/wiki/List_of_highest-grossing_films_in_the_United_States_and_Canada")
filmsURL
#
allTables <- filmsURL %>% html_table(fill = TRUE)
#
# Read "Not adjusted for inflation" table
table1 <- allTables[[1]]
head(table1)
#
# Read "Adjusted for ticket-price inflation" table
table2 <- allTables[[2]]
head(table2)
#
# Second example: 2019 San Francisco Giants Statistics
dataSFG <- read_html("https://www.baseball-reference.com/teams/SFG/2019.shtml")
dataSFG
#
SFGTables <- dataSFG %>% html_table(fill = TRUE)
#
# Read "Team Batting" table
teamBatting <- SFGTables[[1]]
head(teamBatting)
#
# Read "Team Pitching" table
teamPitching <- allTables[[2]]
head(teamPitching)


world_pop <- read_html("https://www.worldometers.info/world-population/population-by-country/")

world_pop <- html_table(world_pop, fill = TRUE)[[1]]

library(janitor)
world_pop |> clean_names() -> pop_df
pop_df
pop_df |> mutate(population_23=as.numeric(gsub(",", "", population_2023))) -> df_pop

df_pop |> rename(country=country_or_dependency) |> select(country, population_23) ->df_pop
df_pop |> arrange(-population_23)

pop_12 <- df_pop |> arrange(-population_23) |> top_n(12) |> mutate(country_code_2=c("CN","IN","US","ID", "PK","NG" ,"BR","BD", "JP", "RU", "MX","ET")) |> 
  mutate(country_code_3=c("CHN","USA","IDN","IND","BRA","NGA" ,"BGD", "PAK", "JPN","RUS", "MEX","ETH")) |> 
  rename(country_name=country)


pop_12 
pop_12 |>
  #dplyr::filter(year == 2021,population_23>120000000)|> 
  #dplyr::filter(grepl("^S", country_name))|>
  dplyr::arrange(-population_23) |>
  dplyr::select(-country_code_3) |>
  dplyr::slice_head(n = 10) |>
  gt() |>
  cols_move_to_start(columns = country_code_2) |>
  fmt_integer() |>
  fmt_flag(columns = country_code_2) |>
  cols_label(
    country_code_2 = "",
    country_name = "Country",
    population_23 = "population_2023"
  ) |> 
  cols_align(align = "center") |> 
  opt_stylize(style = 6, color="green" ) |> 
  opt_table_font(font=google_font("IBM Plex Sans")) |> 
  opt_align_table_header(align="left") |> 
  data_color(columns = population_23, method = "numeric", palette = "Set3") |> 
  tab_stubhead(label="Country") |> 
  tab_source_note(
    source_note = "Source: https://www.worldometers.info/world-population/population-by-country/") |> 
  tab_source_note(source_note = "The population data in year 2023"
  )




