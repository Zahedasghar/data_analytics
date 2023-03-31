Top 10 favorite dplyr commands
##https://datacornering.com/my-top-10-favorite-dplyr-tips-and-tricks/
## favorite 1
## Select and rename as well
require(dplyr)

iris |>
  select(
    SL = Sepal.Length,
    PL = Petal.Length,
    SW = Sepal.Width,
    PW = Petal.Width,
    Species
  ) |> head()


## favorite 2
# rowwise
randu |>
  rowwise() |>
  mutate(min_xy = min(x, y)) |>
  as.data.frame() |>
  head()


## favorite 3
#Rearrange columns quickly with dplyr everything , to move newly created column before others

randu |>
  rowwise() |>
  mutate(min_xy = min(x, y)) |>
  select(min_xy, everything()) |>
  as.data.frame() |>
  head()

## favorite 4
##Drop unnecessary columns with dplyr

iris |>
  select(-contains("Width")) |>
  head()

## favorite 5
## Use dplyr count or add_count instead of group_by and summarize

iris |> count(Species, name = "Species.count") |> head()


## favorite 6
##Replace nested ifelse with dplyr case_when function

airquality |>
  mutate(temp_cat = case_when(Temp > 70 ~ "high",
                              Temp <= 70 & Temp > 60 ~ "medium",
                              TRUE ~ "LOW")) |> head()


## favorite 7
# Execute calculations across columns conditionally with dplyr (max value where is numeric)

iris |> summarise(across(where(is.numeric),max,na.rm=TRUE))



## Here is a text transformation after which every character column contains text with capital letters.

starwars |>glimpse()

starwars |> 
  select(1:5) |> 
  as.data.frame() |> 
  head()

starwars |> 
  mutate_if(is.character,toupper) |>
  select(1:5)|>
  as.data.frame()|>
  head()

## favorite 8

## Filter by calculation of grouped data inside the filter function

mtcars |> count(cyl)

mtcars <- mtcars |> count(cyl) |> filter(n > 10)
mtcars |> as.data.frame()



# favorite 9
## Get top and bottom values by each group with dplyr 

starwars |> select(gender, mass) |>
  group_by(gender) |>
  slice_max(mass, n = 3, with_ties = F) |>
  arrange(gender, desc(mass)) |>
  tidyr::drop_na() |>
  mutate(cat = "top3") |>
  as.data.frame()

## favorite 10 
# Reflow your dplyr code

# ctrl+shift+A 
starwars |> select(gender, mass) |>  group_by(gender) |>  
  slice_max(mass, n = 3, with_ties = F) |>  arrange(gender, desc(mass)) |>
  tidyr::drop_na() |>  mutate(cat = "top3") |>  as.data.frame()




usethis::use_course("https://tinyurl.com/lab-2-class-survey")


library(readxl)
rev_15_16 <- read_excel("C:/Users/92300/OneDrive - Higher Education Commission/R files/Bal_rev_auth/SectorWisepaymentReport(July2015toJune2016).XLS", 
                                                          skip = 8)

rev_16_17 <- read_excel("C:/Users/92300/OneDrive - Higher Education Commission/R files/Bal_rev_auth/SectorWisepaymentReport(July2016toJune2017).XLS", 
                                   skip = 8)
rev_17_18 <- read_excel("C:/Users/92300/OneDrive - Higher Education Commission/R files/Bal_rev_auth/SectorWisepaymentReport(July2017toJune2018).XLS", 
                                   skip = 8)
rev_18_19 <- read_excel("C:/Users/92300/OneDrive - Higher Education Commission/R files/Bal_rev_auth/SectorWisepaymentReport(July2018toJune2019).XLS", 
                                   skip = 8)

rev_19_20<- read_excel("C:/Users/92300/OneDrive - Higher Education Commission/R files/Bal_rev_auth/SectorWisepaymentReport(July2019toJune2020).XLS", 
                                   skip = 8)

rev_20_21<-read_excel("C:/Users/92300/OneDrive - Higher Education Commission/R files/Bal_rev_auth/SectorWisepaymentReport(July2020toJune2021).XLS", 
                                   skip = 8)

rev_21_22 <- read_excel("C:/Users/92300/OneDrive - Higher Education Commission/R files/Bal_rev_auth/SectorWisepaymentReport(July2021toJune2022).XLS", 
                                              skip = 8)
rev_22_23 <- read_excel("C:/Users/92300/OneDrive - Higher Education Commission/R files/Bal_rev_auth/SectorWisepaymentReport(July2022to14Feb2023).XLS", 
                        skip = 8)

rev_15_16 $`Sales Tax On Services`<-as.numeric(rev_15_16$`Sales Tax On Services`)
rev_15_16$Total<-as.numeric(rev_15_16$Total)
rev_15_16 |> glimpse()
rev_16_17 |> glimpse()
rbind(rev_15_16,rev_16_17)

bind_rows(rev_15_16,rev_16_17)

library(readr)
file<-read_csv("C:/Users/92300/OneDrive - Higher Education Commission/R files/Bal_rev_auth", skip=8)


data <- do.call(rbind, lapply
                ("C:/Users/92300/OneDrive - Higher Education Commission/R files/Bal_rev_auth", read.csv, as.is=T, skip = 8, header = FALSE))
