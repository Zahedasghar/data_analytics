library(tidyverse)
library(readxl)
library(janitor)
library(stringr)

gas<- read_excel("C:/Users/92300/Downloads/Copy of Inclass cleaning (1).xlsx")
 names(gas)
names(gas |> clean_names())
  City_Vector <- c("Karachi", "Larkana", "Nawabshah", "Hyderabad", "Sukkur",
                  "Quetta","NAUSHAHRO FEROZ", "Dadu")
 
 gas$city<-str_extract(gas$Address, paste0("(?i)(", paste0(City_Vector, collapse = "|"), ")"))
# 
# View(gas)
# library(janitor)
 gas |> clean_names() -> gas
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
library(gt)
library(gtExtras)
gas |> select(consumption,total_amount_payable) |> gt_plt_summary()
