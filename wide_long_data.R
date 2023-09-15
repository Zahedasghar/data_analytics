library(tidyverse)
#library(readxl)

wide_df <- read_excel("wide_data.xlsx")
wide_df |> glimpse()

wide_df <- wide_df |> mutate(Dec=as.numeric(Dec)) |> na.omit()

wide_df

wide_df |>
  pivot_longer(cols =!year, names_to = "month",values_to = "index_number")  |> 
  mutate(date=ym(paste(year, month, sep = "-"))) -> df_long


df_long |> ggplot(aes(x=date,y=index_number))+
  geom_line(linewidth=1)+ labs(x="", y="index number", title = "Month time seires plot of an index",
                          caption = "source: dummy, @zahedasghar")
                        
read_excel("wide_data.xlsx") |> mutate(Dec=as.numeric(Dec)) |> na.omit() |> 
  pivot_longer(cols =!year, names_to = "month",values_to = "index_number") |> 
  mutate(date=ym(paste(year, month, sep = "-"))) |> ggplot(aes(x=date,y=index_number))+
  geom_line(linewidth=1)+ labs(x="", y="index number", title = "Month time seires plot of an index",
                               caption = "source: dummy, @zahedasghar")+
  theme(plot.title = element_text(size = 15))





camcorder::gg_record(
  dir = 'images',
  width = 12,
  height = 12 * 9 / 16,
  dpi = 300,
  bg = 'white' 
)
  