# Replace the element "1" with the empty string in the teams vector in order to get the teams_clean
#vector with the correct names.
teams <- c("Fal11cons", "Cardinals", "Seah11awks", "Vikings", "Bro11nco", "Patrio11ts") 

teams_clean <- gsub("11", "", teams)

print(teams_clean)

# Output:
# "Falcons" "Cardinals" "Seahawks" "Vikings" "Bronco" "Patriots"
require(ggplot2)


ggplot(iris, aes(x = Petal.Length, y = Petal.Width , color = Species)) +
  geom_point(size = 3,
             alpha = 0.7,
             shape = 16) +
  labs(
    title = "petal length measurements",
    subtitle = "species of iris",
    x = "length (cm)",
    y = "width (cm)",
    color = "species"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())

library(ggtext)


ggplot(iris, aes(x = Petal.Length, y = Petal.Width , color = Species)) +
  geom_point(size = 3,
             alpha = 0.7,
             shape = 16) +
  labs(
    title = "<span style = 'color:tomato;'>petal length</span> measurements",
    subtitle = "species of iris",
    x = "length (cm)",
    y = "width (cm)",
    color = "species"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title = element_markdown())



ggplot(iris, aes(x = Petal.Length, y = Petal.Width , color = Species)) +
  geom_point(size = 3,
             alpha = 0.7,
             shape = 16,
             show.legend = FALSE) +
  scale_color_manual(values = c("cadetblue", "orchid", "tomato"))+
  labs(
    title = "petal length measurements",
    subtitle = "iris species 
    <span style = 'color:cadetblue'>**setosa**</span>, 
    <span style = 'color:orchid;'>**versicolor**</span> and 
    <span style = 'color:tomato;'>**virginica**</span>",
    x = "length (cm)",
    y = "width (cm)",
    color = "species"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.subtitle = element_markdown())






# BUSINESS SCIENCE R TIPS ----
# R-TIP 036 | ggside: side-plots for ggplot ----
#
# 👉 For Weekly R-Tips, Sign Up Here:
#    https://learn.business-science.io/r-tips-newsletter

# LIBRARIES ----

# devtools::install_github("jtlandis/ggside")

library(ggside)
library(tidyverse)
library(tidyquant)

# DATA ----
mpg


# 1.0 GGSIDE ----

# 1.1 Side-Density ----

mpg %>%
  ggplot(aes(hwy, cty, color = class)) +
  geom_point(size = 2, alpha = 0.3) +
  geom_smooth(aes(color = NULL), se=TRUE) +
  geom_xsidedensity(
    aes(
      y    = after_stat(density),
      fill = class
    ),
    alpha    = 0.5,
    size     = 1
    ,
    position = "stack"
  ) +
  geom_ysidedensity(
    aes(
      x    = after_stat(density),
      fill = class
    ),
    alpha    = 0.5,
    size     = 1
    ,
    position = "stack"
  ) +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  labs(title = "Fuel Economy by Vehicle Type" ,
       subtitle = "ggside density",
       x = "Highway MPG", y = "City MPG") +
  theme(
    ggside.panel.scale.x = 0.4,
    ggside.panel.scale.y = 0.4
  )


# 1.2 Side Boxplot w/ Facets ----

mpg %>%
  ggplot(aes(x = cty, y = hwy, color = class)) +
  geom_point() +
  geom_smooth(aes(color = NULL)) +
  geom_xsideboxplot(
    alpha    = 0.5,
    size     = 1
  ) +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  facet_grid(cols = vars(cyl), scales = "free_x") +
  labs(
    title = "Fuel Economy by Engine Size (Cylinders)"
  ) +
  theme(
    ggside.panel.scale.x = 0.4
  )

# 2.0 LEARNING MORE -----

# FREE MASTERCLASS
# - 10 SECRETS TO BECOMING A DATA SCIENTIST
#   https://learn.business-science.io/free-rtrack-masterclass



pkw|> filter(hp>500 & hp<2000, price<5000000, year==2017, 
             company %in% c("FAW", "Honda","Suzuki","Toyota"))|>
  ggplot(aes(x=hp, y=log(price)))+
  geom_point(size = 2, alpha = 0.3) +
  geom_smooth(aes(color = NULL), se=TRUE)+
  geom_xsidedensity(
    aes(
      y    = after_stat(density),
      fill = company
    ),
    alpha    = 0.5,
    size     = 1
    ,
    position = "stack"
  ) +
  geom_ysidedensity(
    aes(
      x    = after_stat(density),
      fill = company
    ),
    alpha    = 0.5,
    size     = 1
    ,
    position = "stack"
  ) +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  labs(title = "Price of cars and engine capacity: used car sales data" ,
       subtitle = "ggside density",
       x = "Highway MPG", y = "City MPG") +
  theme(
    ggside.panel.scale.x = 0.4,
    ggside.panel.scale.y = 0.4
  )

