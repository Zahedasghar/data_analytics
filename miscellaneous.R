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
