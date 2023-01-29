library(pdftools)
#https://www.tidyverse.org/blog/2022/11/dplyr-1-1-0-is-coming-soon/
library(dplyr)
library(clock)
set.seed(12345)
expenses <- tibble(
  id = c(1, 2, 1, 3, 1, 2, 3),
  region = c("A", "A", "A", "B", "B", "A", "A"),
  cost = c(25, 20, 19, 12, 9, 6, 6)
)
expenses
expenses |>
  group_by(region) |>
  summarise(cost = mean(cost))

expenses |>
  summarise(cost = mean(cost), .by = region)

expenses |>
  group_by(id, region) |>
  summarise(cost = mean(cost))
expenses |>
  summarise(cost = mean(cost), .by = c(id, region))

expenses|> mutate(mean=mean(cost),.by=region)

expenses|>slice(2,.by=region)

join_by(x_id == y_id, region)


df1 <- tibble(x_id = c(1, 2, 2), region = c("A", "B", "A"), x = c(5, 10, 4))
df2 <- tibble(y_id = c(2, 1, 2), region = c("A", "A", "C"), y = c(12, 8, 7))


df1 |>
  left_join(df2, join_by(x_id == y_id, region))



library(dplyr)
library(tidylfs)
# Converts `.sav` files to `.Rds` files, to save
# space and for quicker loading
lfs_convert(lfs_directory = "lfs_data_folder/",
            output_directory = "lfs_rds_folder/")

# Compiles into one file.
lfs <- lfs_compile(lfs_directory = "lfs_rds_folder/")



x <- "Geeks for Geeks is Great!"				
#install.packages("stringr")			 
library("stringr")	 
str_sub(x, - 3, - 1)





# install.packages("ggparliament")
library(ggparliament)
# install.packages("tidyverse")
library(tidyverse)

# Data
ru <- election_data %>%
  filter(country == "Russia" & year == 2016)

colour<-ru|>select(colour)
colour1<- c("#0D2C84")
ru_semicircle <- parliament_data(election_data = ru,
                                 type = "semicircle", # Parliament type
                                 parl_rows = 10,      # Number of rows of the parliament
                                 party_seats = ru$seats) # Seats per party

ru_semicircle|>glimpse()
ggplot(ru_semicircle, aes(x = x, y = y, colour = party_short)) +
  geom_parliament_seats() + 
  theme_ggparliament() +
  labs(title = "Russia, 2016") +
  scale_colour_manual(values = ru_semicircle$colour, 
                      limits = ru_semicircle$party_short)  

colour1
colour<-rbind(colour, colour1)

colour


Karachi_semicircle|>glimpse()











seats<-c(89, 86,45,7,3,1,1,3)
seats
party<-c("PPP", "JI", "PTI","PML-N","JUI-F","TLP","MQM-H","IND")

Karachi<-bind_cols(party, seats, colour)
colnames(Karachi)<-c("party","seats", "colour")
Karachi


Karachi_semicircle <- parliament_data(election_data = Karachi,
                                 type = "semicircle", # Parliament type
                                 parl_rows = 10,      # Number of rows of the parliament
                                 party_seats = Karachi$seats) # Seats per party
ru_semicircle|>glimpse()
Karachi_semicircle |>glimpse()
ggplot(Karachi_semicircle, aes(x = x, y = y, colour = party))+
  geom_parliament_seats() + 
  theme_ggparliament() +
  labs(title = "Karachi Election, 2023", caption="Wikipedia, By Zahid Asghar")
+
  scale_colour_manual(values = Karachi_semicircle$colour, 
                      limits = Karachi_semicircle$party) 






library(tidyverse)
#install.packages("showtext")

library(showtext)
#install.packages("camcorder")
library(camcorder)
library(magick)
library(ggimage)

# load fonts
font_add_google("Cinzel Decorative", "cinzel")
font_add_google("Roboto", "roboto")
showtext_auto()

# load data
survivalists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/survivalists.csv')
loadouts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/loadouts.csv')
episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/episodes.csv')
seasons <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/seasons.csv')
#survivalists|>glimpse()
# data wrangling
plot_data <- survivalists |> 
  filter(result == 1) |> 
  select(season, name, days_lasted) |> 
  mutate(name = recode(name, "Jim Baird" = "Jim Baird / Ted Baird")) |> 
  filter(name != "Ted Baird") |> 
  mutate(season = factor(season)) 

# start recording
gg_record(
  dir = file.path("2023", "2023-01-24", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 4, # width of saved image
  height = 6, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# subtitle
st <- "Alone is an American survival series which follows the struggles of 10 individuals as they survive alone in the wilderness for as long as possible using a limited amount of survival equipment. Season 7 winner, Roland Welker, holds the record after surviving for 100 days."

# Process images
# custom function to apply border to circle image with magick
# Code: https://github.com/tashapiro/tanya-data-viz/blob/main/spotify-artists/scripts/generate-image-labels.R
border <- function(im) {
  ii <- magick::image_info(im)
  ii_min <- min(ii$width, ii$height)
  
  img <- magick::image_blank(width = ii_min, height = ii_min, color = "none")
  drawing <- image_draw(img)
  symbols(ii_min/2, ii_min/2, circles = ii_min/2, bg = 'white', inches = FALSE, add = TRUE)
  dev.off()
  
  x = image_composite(image_scale(drawing, "x430"), image_scale(im, "x400"), offset = "+15+15")
  
  x
}

# Images: https://www.distractify.com/p/alone-winners-where-are-they-now
# Image 6: https://www.instagram.com/p/CYpAMGxpok7
# Image 7: https://www.instagram.com/p/CcM04VrLQyA
# Image 8: https://www.instagram.com/p/CgkNf3LpPMx
# plot
ggplot(data = plot_data) +
  geom_segment(mapping = aes(x = 0, 
                             xend = days_lasted,
                             y = season,
                             yend= season),
               colour = "#f4f7f7",
               linewidth = 1) +
  geom_text(mapping = aes(x = days_lasted + 12, 
                          y = season,
                          label = name),
            size = 9,
            hjust = 0,
            family = "roboto",
            colour = "#f4f7f7") +
  geom_image(mapping = aes(x = days_lasted,
                           y = season,
                           image = paste0("2023/2023-01-24/images/", season, ".png")),
             asp = 4.5/6,
             size = 0.12,
             image_fun = border) +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100),
                     limits = c(-5, 150),
                     expand = c(0, 0)) +
  labs(title = "ALONE",
       subtitle = str_wrap(st, 70),
       x = "Days survived",
       y = "Season") +
  theme(axis.text = element_text(family = "roboto",
                                 size = 24,
                                 lineheight = 0.4,
                                 colour = "#f4f7f7"),
        axis.title = element_text(family = "roboto",
                                  size = 24,
                                  lineheight = 0.4,
                                  margin = margin(t = 10),
                                  colour = "#f4f7f7"),
        axis.ticks = element_blank(),
        plot.subtitle = element_text(family = "roboto",
                                     size = 24,
                                     hjust = 0.5,
                                     lineheight = 0.4,
                                     margin = margin(b = 10),
                                     colour = "#f4f7f7"),
        plot.title = element_text(family = "cinzel",
                                  size = 44,
                                  hjust = 0.5,
                                  margin = margin(b = 10),
                                  colour = "#f4f7f7"),
        plot.margin = margin(10, 10, 10, 10),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title.position = "plot",
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = alpha("#f4f7f7", 0.3)),
        plot.background = element_rect(fill = "#203d58", colour = "#203d58"),
        panel.background = element_rect(fill = "#203d58", colour = "#203d58"))

# save gif
gg_playback(
  name = file.path("2023", "2023-01-24","20230124.gif"),
  first_image_duration = 4,
  width = 500, 
  height = 600,
  units = "px",
  last_image_duration = 20,
  frame_duration = .25,
  background = "#203d58"
)



library(ggthemes)
library(tidyverse)
library(gganimate)
#install.packages("WDI")
#install.packages("RJSONIO")
library(RJSONIO)
library(WDI)
library(readr)
#Let's see if we can find a code for export?
#library(wbstat)
#install.packages("wbstat")
#WDIsearch(string='Population, Total', field='name', cache=NULL)

#(There are also codes for Export of Goods and Services for males and females separately)

df<-WDI( indicator=c("GDP per capita"="NY.GDP.PCAP.KD"), start=1960, end=2022, extra =TRUE)
#ESCAPE_plot_html
#saveRDS(df, file="gdp_fertility.rds")

#df_WDI<-readRDS("gdp_fertility.rds")
## Keep only countries
df<-df %>% filter(region!="Aggregates")|>mutate(gdpPercap=`GDP per capita`)
df$gdpPercap<-round(df$gdpPercap,0)

datos2 <- df %>%
  group_by(year) %>%
  arrange(year, desc(gdpPercap)) %>%
  mutate(ranking = row_number()) %>%
  filter(country %in% c("Pakistan", "India","Bangladesh", "Sri Lanka"))




animacion <- datos2 %>%
  ggplot() +
  geom_col(aes(ranking, gdpPercap, fill = country)) +
  geom_text(aes(ranking, gdpPercap, label = gdpPercap), hjust=-0.1) +
  geom_text(aes(ranking, y=0 , label = country), hjust=1.1) + 
  geom_text(aes(x=15, y=max(gdpPercap) , label = as.factor(year)), vjust = 0.2, alpha = 0.5,  col = "gray", size = 20) +
  coord_flip(clip = "off", expand = FALSE) + scale_x_reverse() +
  theme_minimal() + theme(
    panel.grid = element_blank(), 
    legend.position = "none",
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    plot.margin = margin(1, 4, 1, 3, "cm")
  ) +
  transition_states(year, state_length = 0, transition_length = 2) +
  enter_fade() +
  exit_fade() + 
  ease_aes('quadratic-in-out') 
animate(animacion, width = 700, height = 432, fps = 20, duration = 15, rewind = FALSE)


f_gdp %>% 
  ggplot()+aes(x=log(GDP), Fertility, size=population, color=iso3c)+geom_point(show.legend = FALSE)+
  scale_x_log10()+
  labs(x="GDP per capita (constant 2015 USD)",       #add x axis title
       y="Fertility rate, total (births per woman)") #add y axis title


df_sa<-df|>filter(country %in% c("Pakistan", "India","Bangladesh"))
animated_ggplot3<-df_sa %>% 
  ggplot(aes(x=year, y=gdpPercap, color=country,label=country))+
  geom_point(show.legend = TRUE, size=2)+theme_minimal()+
  scale_x_log10()+
  labs(x="year",       #add x axis title
       y="gdp per capita", title = "Year:{frame_time}") +
  transition_time(year)
animated_ggplot3
animate(animated_ggplot3, nframes=350, fps=20) # to make it go slower
df_sa %>% 
  ggplot()+aes(x=year, y=gdpPercap, color=iso3c)+geom_point(show.legend = FALSE)+
  scale_x_log10()+geom_point(label=country)+
  labs(x="GDP per capita (constant 2015 USD)",       #add x axis title
       y="Fertility rate, total (births per woman)")+ #add y axis title
transition_time(year)
 




library(fixest)

iv_reg_2 <- feols(wage ~ 1 | education ~ education_mom, data = wage_df)
summary(iv_reg_2)