library(tidyverse)
library(zoo)
library(tidyquant)

#install.packages("DataEditR")
library(DataEditR)
data("mpg")
mpg
mpg_edit<-data_edit(x=mpg)
mpg_edit
library(devtools)
devtools::install_github("rstudio/DataEditR
                         ", type = "source")
