library(rvest)
library(tidytable)

df1 <- 
  read_html("https://www.psx.com.pk/psx/announcement/financial-announcements") %>% 
  html_elements("body") %>%
  html_table() %>%
  .[[1]]

df1


df2 <- read_html("https://dps.psx.com.pk/") |> 
  html_elements("body") |> 
  html_table() 
