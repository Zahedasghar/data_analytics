## Twitter: @R_by_Ryo
## Website: ryo-n7.github.io
# install.packages("pacman")
library(pacman)
library(scales)
## Packages
pacman::p_load(ggplot2, dplyr, glue, extrafont, ggrepel, magick, ggtext)
loadfonts(quiet = TRUE)

# Example data frame
politicians_data <- data.frame(
  Politician = c("Politician A", "Politician B", "Politician C", "Politician D"),
  StartDate = as.Date(c("2020-01-01", "2018-05-01", "2016-09-01", "2019-03-15")),
  EndDate = as.Date(c("2023-06-01", "2023-02-28", "2021-12-31", NA))
)

ggplot(politicians_data) +
  geom_segment(aes(x = StartDate, xend = EndDate, y = Politician, yend = Politician), size = 5) +
  geom_text(aes(x = StartDate, y = Politician, label = Politician), hjust = -0.1, vjust = 0.5) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +  # Customize x-axis labels
  labs(x = "Year", y = "") +  # Set x-axis and y-axis labels
  theme_minimal()  # Optional: Use a minimalistic theme
# Example data frame
politicians <- c("Politician A", "Politician B", "Politician C")
start_dates <- as.Date(c("2000-01-01", "2015-03-15", "2019-06-01"))
end_dates <- as.Date(c("2002-05-31", "2022-12-31", NA))

timeline_data <- data.frame(
  Politician = politicians,
  StartDate = start_dates,
  EndDate = end_dates
)


ggplot(timeline_data) +
  geom_segment(aes(x = StartDate, xend = EndDate, y = Politician, yend = Politician),
               color = "gray80") +
  geom_point(aes(x = EndDate, y = Politician), color = "steelblue", size = 5) +
  labs(x = "Timeline", y = "") +  # Set x-axis and y-axis labels
  theme_minimal() + # Optional: Use a minimalistic theme
coord_flip()


## load data
lewa_shot_contrib <- readRDS(url("https://raw.githubusercontent.com/Ryo-N7/soccer_ggplots/master/data/lewa_shot_contrib.RDS"))

## Plot
lewa_shot_contrib %>% 
  ggplot(aes(x = opponent)) +
  geom_line(aes(y = xG_xA, group = NA)) +
  geom_point(aes(y = xG_xA, color = match_result), size = 6) +
  geom_hline(yintercept = 0.28) +
  annotate("text", x = 0.75, y = 0.35, hjust = 0, size = 4,
           family = "Roboto Condensed",
           label = "Average Bundesliga Attacker: 0.28 xG + xA") +
  scale_y_continuous(labels = seq(0, 3.5, by = 0.2),
                     breaks = seq(0, 3.5, by = 0.2),
                     limits = c(0, 3.1)) +
  #scale_color_manual(values = fill_cols) +
  labs(title = "<b style='color: red'>Robert Lewandowski</b>  Shot Contribution",
       subtitle = "xG + xA | Bayern München | Hinrunde (2019-2020)",
       x = NULL, y = "xG + xA",
       caption = glue("
                      Data: understat.com
                      Twitter: @R_by_Ryo")) +
  theme_minimal() +
  theme(text = element_text(family = "Roboto Condensed"),
        plot.title = element_markdown(size = 20),
        plot.subtitle = element_text(size = 18),
        plot.caption = element_text(size = 12),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 30),
        legend.position = c(0.1, 0.95),
        legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        panel.grid.minor = element_blank())



## save
ggsave(filename = here::here("Bundesliga 2019-2020/output/lewa_shot_contrib_hinrunde.png"),
       height = 8, width = 12)

## add_logo function from Thomas Mock
add_logo <- function(plot_path, logo_path, logo_position, logo_scale = 10){
  
  # Requires magick R Package https://github.com/ropensci/magick
  
  # Useful error message for logo position
  if (!logo_position %in% c("top right", "top left", "bottom right", "bottom left")) {
    stop("Error Message: Uh oh! Logo Position not recognized\n  Try: logo_positon = 'top left', 'top right', 'bottom left', or 'bottom right'")
  }
  
  # read in raw images
  plot <- magick::image_read(plot_path)
  logo_raw <- magick::image_read(logo_path)
  
  # get dimensions of plot for scaling
  plot_height <- magick::image_info(plot)$height
  plot_width <- magick::image_info(plot)$width
  
  # default scale to 1/10th width of plot
  # Can change with logo_scale
  logo <- magick::image_scale(logo_raw, as.character(plot_width/logo_scale))
  
  # Get width of logo
  logo_width <- magick::image_info(logo)$width
  logo_height <- magick::image_info(logo)$height
  
  # Set position of logo
  # Position starts at 0,0 at top left
  # Using 0.01 for 1% - aesthetic padding
  
  if (logo_position == "top right") {
    x_pos = plot_width - logo_width - 0.01 * plot_width
    y_pos = 0.01 * plot_height
  } else if (logo_position == "top left") {
    x_pos = 0.01 * plot_width
    y_pos = 0.01 * plot_height
  } else if (logo_position == "bottom right") {
    x_pos = plot_width - logo_width - 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.01 * plot_height
  } else if (logo_position == "bottom left") {
    x_pos = 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.01 * plot_height
  }
  # Compose the actual overlay
  magick::image_composite(plot, logo, offset = paste0("+", x_pos, "+", y_pos))
}

## bundesliga logo
plot_logo <- add_logo(plot_path = here::here("Bundesliga 2019-2020/output/lewa_shot_contrib_hinrunde.png"),
                      logo_path = "https://upload.wikimedia.org/wikipedia/en/d/df/Bundesliga_logo_%282017%29.svg",
                      logo_position = "top right",
                      logo_scale = 18)

## save 
magick::image_write(
  image = plot_logo, 
  path = here::here("Bundesliga 2019-2020/output/lewa_shot_contrib_hinrunde_logo.png"))
