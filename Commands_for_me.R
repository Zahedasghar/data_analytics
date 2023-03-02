### Labs for practice 
https://moderndive.github.io/moderndive_labs/

## R for Health Data Science
  https://argoshare.is.ed.ac.uk/healthyr_book/aggregating-group-by-summarise.html


library(help=SuffolkEcon)
## Be the boss of your factors (factor_data_analysis file)
https://stat545.com/factors-boss.html


## Coloring title in ggplot
https://datacornering.com/how-to-use-different-colors-in-the-ggplot2-title-in-r/



## To get commands from Rmd or qmd
library(knitr)
knitr::purl(input="data_wrangling.qmd",output="wrangling.r",documentation = 2)

## To save data
saveRDS(source, file="filename.rds")
readRDS()

## 
ggsave("file.png")

## 
anim_save(".gif")


# dplyr tutorial https://anderfernandez.com/en/blog/dplyr-tutorial/
## gganimate for adjusting speed and other things https://anderfernandez.com/en/blog/dplyr-tutorial/

##Rendering Quarto to other format 
quarto render document.qmd --to acm-html

## Olympic data analysis https://github.com/cosmoduende/r-olympic-games/blob/main/main.r 
write.xlsx(file, "name.xlsx")
# Write.xlsx() Example with sheetName
write.xlsx(df,'/Users/admin/new_file.xlsx', sheetName="Sheet1")

## Journal article

###### A file is downloaded with quarto-journal in RepTemplates
## Command for having JASA format is (run it in terminal)
quarto use template quarto-journals/jasa

## To highlight or blink somelines

```{r}
#| code-line-number: "|4|11|12|"

library(tidyverse)
library(gapminder)
gapminder
library(gt)
gapminder %>% slice(1:6,) %>% gt()



## To convert slides to pdf
renderthis::to_pdf("file.html")



## ggtitle with a color 
Albert Rapp
@rappa753
·
Dec 27
Instead of using a big legend in your plot, try coloring the words in your title.

3 steps to do that with #rstats:

1️⃣ Use Markdown in your title with {ggtext} and theme()
2️⃣ Create a text variable that wraps words into <span color="..."></span>
  3️⃣ Use that variable in labs()