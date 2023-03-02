# Replace the element "11" with the empty string in the teams vector in order to get the teams_clean
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

## After the IMG (Farrukh Saleem)
sect<-c("electricity", "gas","petrol & diesel", "resulting inflation")
rates<-c(20,50,15,35)
library(tidyverse)
library(gt)
library(gtExtras)
## Haemorrhaging
sector2<-c("power (2-decades)","gas (2-decades)","commodity operations (2-decades)",
           "PIA per year)",
           "PSMill per year","textile cartel(subsidies couple of yrs)", "fertilizer cartel(per year)")
Haemorrhaging<-c(2500,1500,800, 67,134,1000,150)  ## Losses to the economy 
hamo<-bind_cols(sector2,Haemorrhaging) ## Combining two columns
colnames(hamo)<-c("sector","Rs_in_bill") ## Rename the two columns
hamo|>gt()|> ## to have a title, subtitle, caption, theme, source
    tab_header(title = 'Haemorrhaging Pak Economy',
      subtitle = 'Exact sources of bleeding are known, but surgeons are missing') |> 
    tab_footnote(footnote = 'Public Sector Enterprizes (PSEs) debt now stands at Rs2 trillion'
    )|>gt_theme_pff()|>tab_source_note("Source : After the IMF by Dr. Farrukh Saleem")
## Loans obtained by Pakistan
trans<-c("United States","Multilaterals", "China", "International bonds", "IMF outstanding with 23 prog")
amount<-c(78.3,60,37,10,7.8)  
transf<-bind_cols(trans,amount)
colnames(transf)<-c("Transfusion provider", "Amount_bill_USD")
transf|>gt()|> tab_header(title = 'Blood transfusion: Pak Haemorrhaging Economy',
                  subtitle = 'No more blood available and urgent need to have surgery to stop haemorrhaging') |> 
  tab_footnote(footnote = 'Are there competent surgeons available?'
  )|>gt_theme_nytimes()|>tab_source_note("Source : After the IMF by Dr. Farrukh Saleem")



library(tidyverse)
require(dataxray)
mpg|>view_xray()
library(dataxray)

diamonds <- diamonds %>% 
  mutate(price = structure(price, label = 'price in US dollars'),
         carat = structure(carat, label = 'weight of the diamond'),
         cut = structure(cut, label = 'quality of the cut (Fair, Good, Very Good, Premium, Ideal)'),
         color = structure(color, label = 'diamond colour, from D (best) to J (worst)'),
         clarity = structure(clarity, label = 'a measurement of how clear the diamond is 
                                               (I1 (worst), SI2, SI1, VS2, VS1, VVS2, VVS1, IF (best))'),
         x = structure(x, label = 'length in mm'),
         y = structure(y, label = 'width in mm'),
         z = structure(z, label = 'depth in mm'),
         depth = structure(depth, label = 'total depth percentage = z / mean(x, y) = 2 * z / (x + y)'),
         table = structure(table, label = 'width of top of diamond relative to widest point'))

diamonds %>% 
  report_xray(data_name = 'Diamonds', study = 'ggplot2')


library(dataxray)

diamonds |>
  make_xray() |> 
  view_xray()
library(palmerpenguins)
penguins|>
  make_xray()|>
  view_xray()
