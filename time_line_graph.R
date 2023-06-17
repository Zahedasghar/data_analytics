library(tidyverse)
library(pacman)
library(scales)
## Packages
pacman::p_load(ggplot2, dplyr, glue, extrafont, ggrepel, magick, ggtext)
loadfonts(quiet = TRUE)

library(readxl)
library(janitor)
library(gt)
library(gtExtras)

finance_min <- read_excel("data/finance-minister.xlsx")
finance_min |> clean_names() |> select(2,3,5,6) -> fmin

fmin |> gt() |> 
    cols_label(
    fin_min="Finance Minister") |> 
  cols_align(align = "center") |> 
  opt_stylize(style = 6, color="green" ) |> 
  opt_table_font(font=google_font("IBM Plex Sans")) |> 
  opt_align_table_header(align="left") |> 
  data_color(columns = policy, method = "numeric", palette = "Set5") |>
    tab_source_note(
    source_note = "Source:FM policies by Shabar Zaidi brecorder")  |> gt_theme_nytimes()



|> gt_theme_pff() |> tab_header(title = "Pakistani FM and their policies") |> 
  tab_footnote("Source: Shabar Zaidi brecorder")


ggplot(fmin) +
  geom_segment(aes(x = start_dates, xend = end_dates, y = fin_min, yend = fin_min),
               color = "gray80") +
  geom_point(aes(x = end_dates, y = fin_min), color = "steelblue", size = 5) +
  labs(x = "Timeline", y = "") +  # Set x-axis and y-axis labels
  theme_minimal()   # Optional: Use a minimalistic theme



cols_label(
           fin_min="Finance Minister") |> 
  cols_align(align = "center") |> 
  opt_stylize(style = 6, color="green" ) |> 
  opt_table_font(font=google_font("IBM Plex Sans")) |> 
  opt_align_table_header(align="left") |> 
  data_color(columns = pop, method = "numeric", palette = "Set3") |> 
  tab_stubhead(label="Country") |> 
  tab_source_note(
    source_note = "Source: Gapminder Dataset.") |> 
  tab_source_note(source_note = "The Population Data in year 2007"
  )









ggplot(fmin) +
  geom_segment(aes(x = start_dates, xend = end_dates, y = fin_min, yend = fin_min, color=end_dates), linewidth = 3) +
  geom_text(aes(x = start_dates, y = fin_min, label = policy), hjust = 0.5, vjust = 1.5) 
+
  scale_x_date(date_labels = "%Y", date_breaks = "10 year") +  # Customize x-axis labels
  labs(x = "Year", y = "") +  # Set x-axis and y-axis labels
  theme_minimal()



)
