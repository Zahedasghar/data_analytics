# BUSINESS SCIENCE R TIPS ----
# R-TIP 039 | grafify: Easy Graphs and ANOVAs ----
#
# 👉 For Weekly R-Tips, Sign Up Here:
#    https://learn.business-science.io/r-tips-newsletter

# LIBRARIES ----

# remotes::install_github("ashenoy-cmbi/grafify@*release", dependencies = T)

library(tidyverse)
library(grafify)

# DATA ----
mpg

# 1.0 GRAPHING 2-VARIABLES ----

# 1.1 Scatterbar SD ----
mpg %>%
  plot_scatterbar_sd(cyl, hwy)

# 1.2 Scatterbox ----
mpg %>%
  plot_scatterbox(cyl, hwy, jitter = 0.2, s_alpha = 0.5)

# 1.3 Dotviolin ----
mpg %>%
  plot_dotviolin(cyl, hwy, dotsize = 0.4, ColPal = "bright")

# 2.0 GRAPHING 3-VARIABLES ----

mpg %>%
  plot_3d_scatterbox(cyl, hwy, class, s_alpha = 0)

# 3.0 BEFORE-AFTER PLOTS ----

mpg %>%
  group_by(model, year) %>%
  summarize(mean_hwy = mean(hwy))%>%
  ungroup()%>%
  plot_befafter_colors(year, mean_hwy, model)


# LEARNING MORE ----

# FREE MASTERCLASS
# - 10 SECRETS TO BECOMING A DATA SCIENTIST
#   https://learn.business-science.io/free-rtrack-masterclass
library(tidyverse)
# 5-COURSE R-TRACK
# - Beginner to Expert in 6-months
#   https://university.business-science.io/p/5-course-bundle-machine-learning-web-apps-time-series/
library(readxl)
inflation<-read_excel("inflation-22.xlsx")
inflation |> glimpse()
inflation<-inflation|>select(`...1`,weight, Date, CPI)
colnames(inflation)<-c("category", "weight", "Date","CPI")
inflation|>slice(1:30) |>filter(category%in% c("General","Transport", "Food & Non-alcohlic Bev."))|>
plot_befafter_colors(Date, CPI, category)

inflation|>slice(1:30) |>
  plot_befafter_colors(Date, CPI, category)





yoy<-inflation|>D
yoy|>glimpse()
# LEARNING MORE ----

# FREE MASTERCLASS

