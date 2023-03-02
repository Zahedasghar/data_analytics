library(googlesheets4)
infstat<- read_sheet('https://docs.google.com/spreadsheets/d/1HgFduprugjTJ8qTSAJFOC-6vWrRhUg-C5X_BX8FObw8/edit?resourcekey#gid=1960622265')

infstat
infstat <- infstat |> slice(21:45)
#View(infstat)
library(janitor)
infstat |> clean_names() |> select(-complete_name) -> infstat

names(infstat)
infstat
infstat |> rename(intro_extro=do_you_consider_yourself_very_social_mixing_freely_with_groups_or_less_social,
                   sleep_duration=how_many_hours_do_you_sleep_at_night_on_average_round_in_hours_like_6_7_8,
                   bed_time=at_what_time_do_you_go_to_bed,
                  cgpa=what_is_your_cgpa_round_to_2_decimal_points,
                   course_fear=how_fearful_you_are_in_this_course_1_least_to_5_most,
                   instructor_fear=how_comfortable_you_feel_to_talk_to_course_teacher_of_inferential_statistics_1_least_comfortable_to_5_highly_comfortable,
                   wt_kg=weight_in_complete_kgs,
                   height_inches=height_round_to_inches_5_feet_3_inches_means_enter_63,
                  
                   ) ->infstat1

infstat1$cgpa <- as.numeric(infstat1$cgpa) 
infstat1$height_inches<- as.numeric(infstat1$height_inches)
infstat1 |> glimpse()

ggplot(infstat1)+aes(x=height_inches,y=wt_kg, colour=gender)+geom_point(size=3)+
  labs(title="Relationship between height and weight")
View(infstat1)
