library(tidyverse)
library(zoo)
library(tidyquant)

#install.packages("DataEditR")
library(DataEditR)
data("mpg")
mpg|>glimpse()
mpg_edit<-data_edit(x=mpg)
mpg_edit
library(devtools)
devtools::install_github("rstudio/DataEditR
                         ", type = "source")
View(mpg)

## 
View(mpg)
mpg |>
  
  select(manufacturer, model, cty, hwy, class) |>
  pivot_longer(cols = c(cty, hwy))|>
  mutate(
    model = fct_reorder(
      str_glue("{manufacturer} {model}") |> str_to_title(),
      value
    ),
    name = str_to_upper(name)
  ) |>
  
  ggplot(aes(x = model, y = value, fill = class)) +
  geom_boxplot() +
  facet_grid(cols = vars(name), scales = "free_y") +
  coord_flip()+
  scale_fill_tq()+
  theme_tq() +
  labs(title = "Fuel Economy by Model", y = "MPG", x = "")
