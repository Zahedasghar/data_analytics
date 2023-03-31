library(tidyverse)
# library(readxl)
# gas<- read_excel("C:/Users/92300/Downloads/Copy of Inclass cleaning (1).xlsx")
# gas |> glimpse()
# View(gas)
# 
# 
# ### Please have a variable name either with under score or with dash
# ## total_amount_payable
# 
# 
# 
# names(gas)
# City_Vector <- c("Karachi", "Larkana", "Nawabshah", "Hyderabad", "Sukkur",
#                  "Quetta","NAUSHAHRO FEROZ", "Dadu")
# 
# library(stringr)
# gas$city<-str_extract(gas$Address, paste0("(?i)(", paste0(City_Vector, collapse = "|"), ")"))
# 
# View(gas)
# library(janitor)
# gas |> clean_names() -> gas
# 
# 
# 
# names(gas)
# 
# g
# saveRDS(gas,file = "gas.rds")

gas<-readRDS("gas.rds")
gas |> glimpse()
library(dataxray)

gas |> make_xray() |> 
  view_xray()

gas |> glimpse()
gas |> select(consumption,total_amount_payable) |> gt_plt_summary()
