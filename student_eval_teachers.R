library(readxl)
library(tidyverse)
library(modelsummary)
library(gtExtras)
library(gtsummary)
teacher_eval<-read_excel("students_evaluation.xlsx")

T_Eval<-as_tibble(teacher_eval)
T_Eval$
T_Eval$`Level of effort [Level of effort you put into the course]`
T_Eval %>% select(`Level of effort [Level of effort you put into the course]`) %>% 
  tbl_summary()

T_Eval %>% filter(Instrcutor_name=="MT") %>% select(`Level of effort [Level of effort you put into the course]`) %>% 
  tbl_summary()

T_Eval %>% group_by(Instrcutor_name) %>% select(`Level of effort [Level of effort you put into the course]`) %>% 
  tbl_summary()

T_Eval %>% filter(Instrcutor_name=="SS") %>% tbl_summary()
T_Eval %>% filter(Instrcutor_name=="AZ") %>% datasummary_skim(type = "categorical")


trial %>%
  select(trt, grade, stage, age, marker) %>%
  tbl_strata(strata=stage,
             ~tbl_summary(.x, by = grade , missing = "no"),
             statistic = list(all_continuous()~ "{mean} ({sd})" 
             ))%>%
  bold_labels()



trial %>%
  select(trt, grade, stage, age, marker) %>%
  tbl_strata(
    strata = trt, 
    function(data) {
      data %>%
        tbl_strata(
          strata = stage,
          ~ tbl_summary(
            .x,
            by = grade,
            statistic = all_continuous() ~ "{mean} ({sd})",
            missing = "no"
          ) %>%
            modify_header(all_stat_cols() ~ "**{level}**")
        )
    },
    .combine_with = "tbl_stack",
    .combine_args = list(group_header = c("Drug A", "Drug B"))
  ) %>% 
  bold_labels()




