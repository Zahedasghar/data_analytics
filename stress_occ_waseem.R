library(readxl)
library(tidyverse)
library(janitor)
str_ocu <- read_excel("data/stress_occupation_waseem.xlsx")
str_ocu |> clean_names() |> rename(oc10=x0c10, stess9=stress9) ->str_occ
View(str_ocu)
str_occ |> glimpse()        

stress <- c(1, 2, 3, 4, 5)

  
str_occ |>  
  mutate_at(.vars = vars(contains("stress"))
    , .funs = ~dplyr::recode(
      .x  = .
      , "Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"
    )
  ) -> str_occ

str_occ |> glimpse()

oc <- c(1,2,3,4,5)
str_occ |>  
  mutate_at(.vars = vars(contains("oc"))
            , .funs = ~dplyr::recode(
              .x  = .
              , "Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"
            )
  ) -> str_occ

t <- c(1,2,3,4,5)

str_occ |>  
  mutate_at(.vars = vars(starts_with("t"))
            , .funs = ~dplyr::recode(
              .x  = .
              , "Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"
            )
  ) -> str_occ

str_occ |> glimpse()

age<-c(1,2,3,4)

str_occ |>  
  mutate_at(.vars = vars(contains("age"))
            , .funs = ~dplyr::recode(
              .x  = .
              , "18-29", "30-39", "40-49", "50+"
            ) ) -> str_occ




str_occ$gender<- factor(str_occ$sex, 
                                 levels=c(1,2), 
                                 labels=c("male","female"))

str_occ$marital_status<-factor(str_occ$marital_status,
                               levels = c(1,2,3),
                               labels=c("married",'divorced',"single"))

str_occ$qualification<- factor(str_occ$qualification,
                               levels = c(1,2,3),
                               labels=c("intermediate","graduate","others"))


str_occ$experience<- factor(str_occ$experience,
                               levels = c(1,2,3,4,5),
                               labels=c("< 1 yr","1-5","6-10","11-15","> 15 yrs"))

## All variables into factors
str_occ<-str_occ |> mutate_all(as.factor)
str_occ |> glimpse()

str_occ |> select(gender, stress1) |> tbl_summary(by=gender) |> add_p()

str_occ |> select(gender, stress2) |> tbl_summary(by=gender)|> add_p()

str_occ |> select(qualification, stress2) |> tbl_summary(by=qualification)|> add_p()

str_occ |> select(marital_status, stress2) |> tbl_summary(by=marital_status)|> add_p()

str_occ |> select(marital_status, stress3) |> tbl_summary(by=marital_status)
