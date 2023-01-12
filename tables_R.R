library(gapminder)
gapminder
library(gt)
gapminder %>% slice(1:6,) %>% gt()

gapminder %>% slice(1:10,) %>% gt(rowname_col = "row")
gapminder %>% slice(1:10,)%>% gt(rowname_col = "row", groupname_col = "group")


mtcars %>%
  dplyr::slice_head(n = 10) %>%
  gt(rownames_to_stub = TRUE)
gapminder %>%
  dplyr::slice_head(n = 10) %>%
  gt(rownames_to_stub = TRUE)

  exibble %>%
  gt(
    rowname_col = "row",
    groupname_col = "group"
  )

https://rpubs.com/esimms/gt_01_es



sp500 %>% 
  gt_preview(
    bottom_n = 5,
    incl_rownums = FALSE
  )
sp500


library(DT)
data("mtcars")
datatable(
  data = mtcars,
  caption = “Table”,
  filter = “top”
)
