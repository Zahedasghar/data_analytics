library(readxl)
library(tidyverse)
library(janitor)

inflation_table <- read_excel("data/inflation-table.xlsx", 
                              skip = 5)

names(inflation_table)

inflation_table |> glimpse()


inflation_table |> rename("date"=1) -> inf
inf_df |> glimpse()

 inf |> 
  gather(key = "variable", value = "value", -date) -> inf_df
 
 
 inf_df |> 
   mutate(category = case_when(
     endsWith(variable, "...2") ~ "general",
     endsWith(variable, "...3") ~ "general",
     endsWith(variable, "...4") ~ "urban",
     endsWith(variable, "...5") ~ "urban",
     endsWith(variable, "...6") ~ "rural",
     endsWith(variable, "...7") ~ "rural",
     endsWith(variable, "...8") ~ "food_u",
     endsWith(variable, "...9") ~ "food_u",
     endsWith(variable, "...10") ~ "food_r",
     endsWith(variable, "...11") ~ "food_r",
     endsWith(variable, "...12") ~ "nfne_u",
     endsWith(variable, "...13") ~ "nfne_u",
     endsWith(variable, "...14") ~ "nfne_r",
     endsWith(variable, "...15") ~ "nfne_r",
     endsWith(variable, "...16") ~ "spi",
     endsWith(variable, "...17") ~ "spi",
     endsWith(variable, "...18") ~ "wpi",
     endsWith(variable, "...19") ~ "wpi"   )) ->inf_df
 View(inf_df)

 inf_df |> mutate(urban_rural=case_when(
   startsWith(variable,"YoY")~"yearly",
   startsWith(variable,"MoM")~"monthly" )) -> inf_df
 
 
 
 dirty_data <- read_excel("data/dirty_data.xlsx", skip=1, .name_repair = make_clean_names)

dirty_data |> glimpse()  
roster <- dirty_data |>  remove_empty(c("rows", "cols")) |>
  remove_constant(na.rm = TRUE, quiet = FALSE) |> # remove the column of all "Yes" values 
  mutate(hire_date = convert_to_date(hire_date, # handle the mixed-format dates
                                     character_fun = lubridate::mdy),
         cert = dplyr::coalesce(certification, certification_2)) |>
  select(-certification, -certification_2) 

roster %>% get_dupes(contains("name"))

## tabyl
library(gt)
tabyl(roster, subject) 

## Two variables

roster %>%
  filter(hire_date > as.Date("1950-01-01")) %>%
  tabyl(employee_status, full_time)
## Three variables
roster %>%
  tabyl(full_time, subject, employee_status, show_missing_levels = FALSE)


## Adorn table 
roster %>%
  tabyl(employee_status, full_time) %>%
  adorn_totals("row") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting() %>%
  adorn_ns() %>%
  adorn_title("combined")
