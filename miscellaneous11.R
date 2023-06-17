library(tidyverse)
library(gt)

filtered_penguins <- palmerpenguins::penguins |> 
  filter(!is.na(sex))

penguin_weights <- palmerpenguins::penguins |> 
  filter(!is.na(sex)) |> 
  group_by(species) |> 
  summarise(
    Min = min(body_mass_g),
    Mean = mean(body_mass_g) |> round(digits = 2),
    Max = max(body_mass_g)
  ) |> 
  mutate(species = as.character(species), Distribution = species) |> 
  rename(Species = species)

plot_density_species_gradient <- function(species, variable) {
  full_range <- filtered_penguins |> 
    pull({{variable}}) |> 
    range()
  
  
  
  filtered_penguins |> 
    filter(species == !!species) |> 
    ggplot(aes(x = {{variable}}, y = species)) +
    geom_violin(fill = 'white', col = 'black', size = 2) +
    theme_minimal() +
    scale_y_discrete(breaks = NULL) +
    scale_x_continuous(breaks = NULL) +
    labs(x = element_blank(), y = element_blank()) +
    coord_cartesian(xlim = full_range)
}

penguin_weights |> 
  gt(id = 'my_tbl') |> 
  tab_spanner(
    label = 'Penguin\'s weight',
    columns = -Species
  ) |> 
  text_transform(
    locations = cells_body(columns = 'Distribution'),
    # Create a function that takes the a column as input
    # and gives a list of ggplots as output
    fn = function(column) {
      map(column, ~plot_density_species_gradient(., body_mass_g)) |> 
        ggplot_image(height = px(50), aspect_ratio = 3)
    }
  ) |> 
  cols_align(
    align = 'center',
    columns = 'Distribution'
  ) |> 
  tab_options(
    table.font.names = 'Merriweather',
    table.font.color = 'white',
    #table.font.weight = 'bold',
    heading.align = 'left',
    table_body.hlines.style = 'dashed', 
    table_body.hlines.width = px(1), 
    table_body.hlines.color =  'white',
    table_body.border.top.color = 'white',
    table_body.border.top.style = px(1),
    heading.border.bottom.width = px(1), 
    heading.border.bottom.color =  'white',
    column_labels.border.bottom.width = px(1), 
    column_labels.border.bottom.color =  'white',
    column_labels.font.weight = 'bold',
    table.border.top.style = 'none',
    table_body.border.bottom.color = 'white'
  ) |> 
  opt_css(
    css = '
    .gt_table {
      background: linear-gradient(-135deg, #c31432, #240b36);
    }
    
    #my_tbl .gt_heading, #my_tbl .gt_col_heading, #my_tbl .gt_column_spanner_outer {
      background:transparent;
    }
    
    #my_tbl .gt_col_headings {
      border-top-color: white;
      border-top-width:1px;
    }
    
    '
  )



library(palmerpenguins)

 
palmerpenguins::penguins |> dplyr::filter(!is.na(sex))




library(tidyverse)
library(readxl)
xport <- read_excel("data/export-import-2023.xlsx")
xport |> glimpse()
ggplot(xport, aes(x=date)) +
  geom_line(aes(y=Imports),color='red')+
  geom_line(aes(y=Exports),color='steelblue')+
  geom_line(aes(y=Remitances), color='blue')
library(janitor)

g1 <- ggplot(xport, aes(x = date)) +
  geom_line(aes(y = Imports), color = 'red', linewidth=1.2) +
  geom_line(aes(y = Exports), color = 'steelblue',linewidth=1.2) +
  geom_line(aes(y = Remitances), color = 'blue',linewidth=1.2,linetype="twodash")

g1 + theme_minimal()
  
xport |> clean_names() -> trade
trade
ggplot(trade, aes(x = date)) +
  geom_line(aes(y = trade_deficit), color = 'red', linewidth=1.2) 
trade |> 
  pivot_longer(cols = c(2:ncol(trade)),
               names_to = 'category',
               values_to = 'values') -> long_data

long_data |> filter(category %in% c('imports', 'exports', 'remitances')) |> 
  ggplot(aes(x=date,y=values))+geom_line(aes(color = category, linetype = category),
                                         linewidth=1.0) + 
  scale_color_manual(values = c("darkred", "steelblue", "black"))


long_data |> filter(category %in% c( 'exports', 'remitances')) |> 
  ggplot(aes(x=date,y=values))+geom_line(aes(color = category, linetype = category),
                                         linewidth=1.0) + 
  scale_color_manual(values = c("darkred", "steelblue", "black"))
