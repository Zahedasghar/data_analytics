library(tidyverse)
load("D:/RepTemplates/mixture/data/anes20.rda")
anes20 |> glimpse()

anes20 |> group_by(V201320x) |> count() 

# Show it for first 50 cases

anes20 |> slice(1:50) |> group_by(V201320x) |> count()


#Add up the category frequencies and store them in a new object, "sample_size"  



sample_size<-2560+1617+3213+446+389 
#Print the value of "sample_size"
sample_size
