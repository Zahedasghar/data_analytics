library(dplyr)
library(plotly)
fig <- iris |> 
  plot_ly(x = ~Species, 
          y = ~Sepal.Length, 
          split = ~Species,
          type = "violin") |> 
  layout(
    title = "Custom Buttons in plotly in R",
    updatemenus = list(
      list(
        type = "buttons",
        direction = "bottom",
        pad = list("r" = 0, "t" = 10, "b" = 10),
        buttons = list(
          list(
            method = "restyle",
            args = list("type","violin"),
            label = "Violin Plot"
          ),
          list(
            method = "restyle",
            args = list("type","box"),
            label = "Box Plot"
          ),
          list(
            method = "restyle",
            args = list("type","bar"),
            label = "Bar Plot"
          )
        )
      )
    )
  )

fig
