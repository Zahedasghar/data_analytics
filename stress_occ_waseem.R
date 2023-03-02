library(readxl)
library(tidyverse)
library(janitor)

str_ocu <- read_excel("data/stress_occupation_waseem.xlsx") 

str_ocu |> clean_names()  ->str_occ 

str_occ |> rename(stress9=stess9, oc10=x0c10 ) -> str_occ

View(str_occ)


stress <- c(1, 2, 3, 4, 5)

  
str_occ |>  
  mutate_at(.vars = vars(contains("stress"))
    , .funs = ~dplyr::recode(
      .x  = .
      , "Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"
    )
  ) -> str_occ


str_occ |> glimpse()  

oc<-c(1,2,3,4,5)

str_occ |>  
  mutate_at(.vars = vars(contains("oc"))
            , .funs = ~dplyr::recode(
              .x  = .
              , "Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"
            )
  ) -> str_occ

t<- c(1,2,3,4,5)


str_occ |>  
  mutate_at(.vars = vars(starts_with("t"))
            , .funs = ~dplyr::recode(
              .x  = .
              , "Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"
            )
  ) -> str_occ

str_occ |> glimpse()

str_occ <- str_occ |> case_match(age=age, 1 ~ "18-29", 2 ~ "30-39",3~"40-49", 4~"50+") 
str_occ |> select(sex, stress1) |> tbl_summary(by=sex) |> add_p()

str_occ <- str_occ |>  mutate(age=recode(age,'1' ="18-29", '2' = "30-39",'3'="40-49", '4'="50+"))

str_occ <- str_occ |>  mutate(gender=recode(sex,'1' ="male", '2' = "female"))

str_occ <- str_occ |>  mutate(marital_status=recode(marital_status,'1' ="married", '2' = "divorced",
                                                    '3'="single"))

library(gtsummary)

