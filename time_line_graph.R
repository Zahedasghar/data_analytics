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
finance_min
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

fmin |>  
  mutate(fin_min = as_factor(fin_min)) |> 
ggplot()+
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



library(scales)
library(ggthemes)


fmin |>  
  mutate(fin_min = as_factor(fin_min)) |> 
  ggplot() +
  geom_segment(aes(x = start_dates, xend = end_dates, y = fin_min, yend = fin_min, color=fin_min), linewidth = 3) +
  geom_text(aes(x = start_dates, y = fin_min, label = policy, size=1),size=3.0, hjust = 0.3, vjust = 1.5) +
  #scale_x_date(date_labels = "%Y", date_breaks = "10 year") +  # Customize x-axis labels
  labs(x = "Year", y = "",
       caption = "ShabarZaidi Brecorder") +  # Set x-axis and y-axis labels
  theme_minimal()+theme_fivethirtyeight() +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 24))+
  theme(legend.position = "none",
        #legend.title = element_blank(),
        #legend.direction = "vertical",
        #axis.title.y = element_blank(),
        text = element_text(size = 10)) +
  labs(title = "Pakistani Finance Ministers' Policy ",
       subtitle = "From 1948 to-date, except ZAB in 1970s, all have followed US dominated agenda or completely US agenda") +
  theme(plot.title = element_text(size = 20))












ggsave(
  filename  = "timeline.png",
   #device    = cairo_pdf
   path      = NULL
  , scale     = 1
  , width     = 15.69
  , height    = 12.27
  , units     = c("in", "cm", "mm")[1]
  , dpi       = 600
  , limitsize = TRUE
)



theme_set(theme_bw())
library(sysfonts)
library(showtextdb)
library(showtext)


fminplt <- fmin |>  
  mutate(fin_min = as_factor(fin_min)) |> 
  ggplot() +
  geom_segment(aes(x = start_dates, xend = end_dates, y = fin_min, yend = fin_min, color=fin_min), linewidth = 3) +
  geom_text(aes(x = start_dates, y = fin_min, label = policy, size=0.7),size=3.0, hjust = 0.3, vjust = 1.5) +
  #scale_x_date(date_labels = "%Y", date_breaks = "10 year") +  # Customize x-axis labels
  labs(x = "Year", y = "",
       caption = "ShabarZaidi Brecorder") +  # Set x-axis and y-axis labels
  theme_minimal()+theme_fivethirtyeight() +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 24))+
  theme(legend.position = "none",
        #legend.title = element_blank(),
        #legend.direction = "vertical",
        #axis.title.y = element_blank(),
        text = element_text(size = 10)) +
  labs(title = "Pakistani Finance Ministers' Policy ",
       subtitle = "From 1948 to-date, except ZAB in 1970s, all have followed US dominated agenda or completely US agenda") +
  theme(plot.title = element_text(size = 20))

library(ragg)
ragg::agg_png("time_line.png", width = 30, height = 25, units = "in", res = 300, scaling = 2)
fminplt
dev.off()



prime_min <- read_excel("data/finance-minister.xlsx", sheet=2)
prime_min |> glimpse()
prime_min |> clean_names()  -> pm_min
pm_min
library(ggplot2)
library(ggthemes)
pm_min |> 
  mutate(PM = as_factor(prime_minister)) |> 
  ggplot() +
  geom_segment(aes(x = start_date, xend = end_date, y = PM, yend = PM, color=prime_minister), linewidth = 3) +
  geom_text(aes(x = start_date, y = PM, label=PM, size=0.7),size=3.0, hjust = 0.3, vjust = 1.5) +
  #scale_x_date(date_labels = "%Y", date_breaks = "10 year") +  # Customize x-axis labels
  labs(x = "Year", y = "",
       caption = "Newspapers") +  # Set x-axis and y-axis labels
  theme_minimal()+theme_fivethirtyeight() +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 24))+
  theme(legend.position = "none",
        #legend.title = element_blank(),
        #legend.direction = "vertical",
        #axis.title.y = element_blank(),
        text = element_text(size = 15)) +
  labs(title = "Timeline of former prime ministers arrested in Pakistan ",
       subtitle = "Imran Khan is the 8th one to face jail since 1962. Benazir Bhutto and Nawaz Sharif faced twice") +
  theme(plot.title = element_text(size = 30))

library(ragg)
plt2 <- pm_min |> 
  mutate(PM = as_factor(prime_minister)) |> 
  ggplot() +
  geom_segment(aes(x = start_date, xend = end_date, y = PM, yend = PM, color=prime_minister), linewidth = 3) +
  geom_text(aes(x = start_date, y = PM, label=PM, size=2),size=5.0, hjust = 0.3, vjust = 1.5) +
  #scale_x_date(date_labels = "%Y", date_breaks = "10 year") +  # Customize x-axis labels
  labs(x = "Year", y = "",
       caption = "Source: Various Newspapers by @zahedasghar") +  # Set x-axis and y-axis labels
  theme_minimal()+theme_fivethirtyeight() +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 24))+
  theme(legend.position = "none",
        #legend.title = element_blank(),
        #legend.direction = "vertical",
        #axis.title.y = element_blank(),
        text = element_text(size = 15)) +
  labs(title = "Timeline of former prime ministers arrested in Pakistan ",
       subtitle = "Imran Khan is the 8th one to face jail since 1962. Benazir Bhutto and Nawaz Sharif faced twice. \n 4 PMs were put behind bars during Imran Khan'  tenure as PM") +
  theme(plot.title = element_text(size = 30))

ragg::agg_png("PM_time_line.png", width = 30, height = 25, units = "in", res = 300, scaling = 2)
plt2
dev.off()
