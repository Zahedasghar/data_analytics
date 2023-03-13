library(readxl)
library(tidyverse)
library(gt)
library(gtExtras)
library(gtsummary)
library(skimr)
ossc <- read_excel("data/ossc.xlsx")

ossc |> mutate(percentage_of_out_of_school_children_age_5_to_16_years_female=as.numeric(percentage_of_out_of_school_children_age_5_to_16_years_female)) -> ossc

ossc |> glimpse()

library(janitor)

ossc |> clean_names() -> ossc



ossc |> select(province, district,ossc_f_16=percentage_of_out_of_school_children_age_5_to_16_years_female,
               ossc_m_16=percentage_of_out_of_school_children_age_5_to_16_years_male,
               ossc_total_16=percentage_of_out_of_school_children_age_5_to_16_years_total) -> ossc_sm
ossc_sm <- ossc_sm |> mutate(ossc_f_16=as.numeric(ossc_f_16))

ossc_sm |> na.omit() |> summarise(
  ossc_total = mean(ossc_total_16) * 100,
  ossc_male = mean(ossc_m_16) * 100,
  ossc_female = mean(ossc_f_16) * 100,
  .by = province
) |> gt() |>
  fmt_number(
    columns = 2:4,
    decimals = 2)   -> ossc_tbl

ossc_tbl

ossc_tbl |> gt_theme_nytimes()|>  
  gt_highlight_rows(rows = 3, font_weight = "normal") |> 
  tab_header(title = "Average percentage of out of school children aged 5-16 in each province")

ossc_sm <- ossc_sm |> mutate(ossc_f_16=ossc_f_16*100,ossc_m_16=ossc_m_16*100)
ossc_sm |> filter(province=="Punajb") |>   select(is.numeric) |> gt_plt_summary()



ossc_sm <- ossc_sm |> rename(Female=ossc_f_16,Male=ossc_m_16) 
ossc_sm %>%
  select(province, district, Male, Female)%>% #select columns of interest
  mutate(diff = Female-Male) %>% #calculate difference
  pivot_longer(cols = c(Male, Female)) |>  #get into long format
rename(Enrollments=value, Gender=name) -> dat_gender

dat_gender|> glimpse()

dat_gender<- dat_gender |> filter(province=="Punajb") 

Males <- dat_gender |> filter(Gender=="Male")

Females <- dat_gender |> filter(Gender=="Female")
dat_gender

p <-   ggplot(dat_gender)+
  
  geom_segment(data = Males,
               aes(x = Enrollments, y = district,
                   yend = Females$district, xend = Females$Enrollments), #use the $ operator to fetch data from our "Females" tibble
               color = "#aeb6bf",
               size = 4.5, #Note that I sized the segment to fit the points
               alpha = .5) +
  
  geom_point(aes(x = Enrollments, y = district, color = Gender), size = 4, show.legend = TRUE)+
  
  ggtitle("Out of school children (%) difference between male and female ")
p+theme_minimal()



dat_gender %>%
  group_by(Gender) %>%
  summarise(mean = mean(Enrollments),
            SE = sd(Enrollments)) %>%
  mutate(meanpos = mean + 1 *SE,
         meanneg = mean - 1 *SE)-> stats
stats_males <- stats %>%
  filter(Gender == "Males")
stats_females <- stats %>%
  filter(Gender == "Females")
head(stats)

dat_gender

diff <- dat_gender %>% 
  filter(Gender == "Male") %>% #you can chose Males or Females, doesn't matter
  mutate(x_pos = Enrollments + (diff/2)) #x position of label (Enrollment value of Males + diff/2)
head(diff)













p + 
  geom_text(data = diff,
            aes(label = paste("D: ",diff), x = x_pos, y = district), #note thatI changed the greek letter Delta to "D:" because of encoding reasons
            fill = "white",
            color = "#4a4e4d",
            size = 2.5,
            family = "Segoe UI Semibold") -> p_labelled
p_labelled+theme_minimal()+
  labs(x="%age of out of school children", y=" ")






p_labelled +
  
  #add facet for more control
  facet_grid(district ~ ., scales = "free", switch = "y") +
  
  #theming
  theme_minimal()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "#4a4e4d"),
        text = element_text(family = "Segoe UI Semibold", color = "#4a4e4d"),
        strip.text.y.left  = element_text(angle = 0),
        panel.background = element_rect(fill = "white", color = "white"),
        strip.background = element_rect(fill = "white", color = "white"),
        strip.text = element_text(color = "#4a4e4d", family = "Segoe UI"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.spacing = unit(0, "lines"),
        plot.margin = margin(1,1,.5,1, "cm"))-> p_ext_facetted
p_ext_facetted


ggplot(dat_gender)+
  
  #add mean and standard deviation for both groups
  geom_rect(xmin = stats_males$meanneg, xmax = stats_males$meanpos,
            ymin = district, ymax = district, fill = "#762a83", alpha = .05)+
  geom_vline(xintercept = stats_males$mean, linetype = "solid", size = .5, alpha = .8, color = "#762a83")+
  
  geom_rect(xmin = stats_females$meanneg, xmax = stats_females$meanpos,
            ymin = district, ymax = district, fill = "#009688", alpha = .05)+  
  geom_vline(xintercept = stats_females$mean, color = "#009688", linetype = "solid",  size = .5, alpha = .8) +
  
  #add point range
  geom_segment(data = Males, aes(x = Enrollments, y = district, yend = Females$district, , xend = Females$Enrollments),
               color = "#aeb6bf", size = 4.5, alpha = .5) +
  
  #add points
  geom_point(aes(x = Enrollments, y = Year, color = Gender), size = 4, show.legend = FALSE) +
  
  #color points
  scale_color_manual(values = c("#009688","#762a83"))+
  #add point-range labels
  geom_text(data = diff, aes(label = paste("D: ",diff), x = x_pos, y = Year), fill = "white", color = "#4a4e4d", size = 2.5, family = "Segoe UI") +
  
  #add annotations for mean and standard deviations
  geom_text(x = stats_females$mean , y = district, label = "MEAN", angle = 90, size = 2.5, color = "#009688", family = "Segoe UI")+
  geom_text(x = stats_females$meanpos , y = district, label = "STDEV", angle = 90, size = 2.5, color = "#009688", family = "Segoe UI")+
  
  #add facets for more control
  facet_grid(Year ~ ., scales = "free", switch = "y") +
  #add title
  ggtitle("Enrollment Trends at Historically Black Colleges and Universities")+
  #theming
  theme_minimal()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "#4a4e4d"),
        text = element_text(family = "Segoe UI", color = "#4a4e4d"),
        strip.text.y.left  = element_text(angle = 0),
        panel.background = element_rect(fill = "white", color = "white"),
        strip.background = element_rect(fill = "white", color = "white"),
        strip.text = element_text(color = "#4a4e4d", family = "Segoe UI"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.spacing = unit(0, "lines"),
        plot.margin = margin(1,1,.5,1, "cm")) -> p_styled
p_styled















ossc_sm |> mutate(province=as.factor(province)) -> ossc_sm
ossc_sm

dat1 <- ossc_sm |> na.omit() |> 
  mutate(
    # ID that is shared for boxplots (this one uses factors, i.e. numbers, as ID instead of continents)
    id = as.numeric(province),
    continent = forcats::fct_reorder(province, Female)
  ) 
dat1
selected_year <- 2007
color_palette <- thematic::okabe_ito(5)
names(color_palette) <- unique(dat1$province)

base_size <- 24
box_stats1 <- dat1 |> 
 group_by(province) |> 
  summarise(
    n = n(), 
    iqr = IQR(Female) |> round(2), 
    range = paste(range(Female) |> round(2), collapse = ' - '),
    mean = mean(Female) |> round(2)
  )
box_stats1

library(ggiraph)

tail(dat1 |> 
  full_join(box_stats1))

box_plot1 <- dat1 |> 
  full_join(box_stats1) |> 
  ggplot(aes(x = Female, y = province, fill = province, data_id = id)) +
  geom_boxplot_interactive(
    aes(
      tooltip = glue::glue(
        '
        {levels(dat1$province)[province]}\n
        {n} districts\n
        Mean Out of School: {mean}\n
        Range: {range}\n
        IQR: {iqr}
        '
      ),
      onclick = glue::glue('window.open("http://en.wikipedia.org/wiki/{levels(dat$continent)[continent]}")')
    ),
    position = position_nudge(y = 0.25), 
    width = 0.5
  ) +
  geom_point_interactive(
    aes(col = province),
    position = position_nudge(y = -0.2),
    size = 11,
    shape = '|',
    alpha = 0.75
  ) +
  scale_fill_manual(values = color_palette) +
  scale_color_manual(values = color_palette) +
  labs(
    x = 'Out of School (in %)',
    y = element_blank(),
    title = glue::glue('Female Children Out of School  (aged 5-16 years) in Pakistan  ')
  ) +
  theme_minimal(base_size = base_size) +
  theme(
    text = element_text(
      color = 'grey20'
    ),
    legend.position = 'none',
    panel.grid.minor = element_blank(),
    plot.title.position = 'plot'
  ) 

box_plot1

gg <- girafe(
  ggobj = box_plot1,
  options = list(
    opts_hover(css = ''),
    opts_hover_inv(css = "opacity:0.1;"),
    opts_tooltip(offx = 25, offy = 0, css = 'background:black;color:white;font-size:16pt;font-family:Open Sans;')
  ),
  height_svg = 9,
  width_svg = 16
)

gg
library(htmltools)
htmltools::save_html(gg,
                     file = "ossc.html")

