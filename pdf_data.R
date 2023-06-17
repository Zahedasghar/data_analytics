library(pdftools)
library(stringi)
library(tidyverse)
library(janitor)
library(DT)
library(data.table)
library(tabulizer)
# https://tellingstorieswithdata.com/09-clean_and_prepare.html

cpi_pdf <-
  pdf_text(pdf = "CPI_Review_May_2023.pdf")

Data1 <-
  extract_tables(
    file  = "CPI_Review_May_2023.pdf"
    , pages = 8
    , area  = list(c(180, 84, 470, 525)) # (top, left, bottom, right)
    , guess = FALSE
  ) [[1]] %>%
  as.data.table()

Data1

# From Bob Rudis: https://stackoverflow.com/a/47793617
cpi_pdf_page_8 <- stri_split_lines(cpi_pdf[[8]])[[1]]
cpi_pdf_page_8
cpi_pdf_raw <- tibble(all=cpi_pdf_page_8[9:42])
#write_csv(cpi_pdf_raw, file = "cpi_pdf_raw.csv")

# cpi_pdf_raw|> glimpse()  # All the columns have been collapsed into one

# Separate columns
# cpi_pdf_raw <-
 

 
  
  
 cpi_pdf_raw |>  
   mutate(all = str_squish(all)) |>
   #mutate(all = str_replace(all, "Age group", "Age-group")) |>
  separate(
    col = all,
    into = c(
      "weightage",
      "may_23", "april_23", "may_22", "april_22", "april_2023","may_2022"
    ),
    sep = "",
    remove = TRUE,
    fill = "right",
    extra = "drop"
  )

cpi_pdf_raw







eco_survey <- pdf_text(pdf = "supplement_2021_22.pdf")
eco_df <- stri_split_lines(eco_survey[[10]])[[1]]
eco_df <- tibble(eco_df[1:51])




agri <- stri_split_lines(eco_survey[[37]])[[1]]
agri_df <- tibble(all=agri[6:28])
agri_df |> filter(agri_df!="") -> df_agri
df_agri

dplyr::mutate(agri_df, str_squish(agri_df, all)) 
df_agri|> mutate(all = str_squish(all)) |>
 # mutate(all = str_replace(all, "Age group", "Age-group")) |>
      separate(
    col = all,
    into = c(
      "year", 
      "wheat", "rice", "sugarcane",
      "maize", "gram", "cotton"
    ),
    sep = " ",
    remove = TRUE,
    fill = "right",
    extra = "drop"
  ) |> View()
