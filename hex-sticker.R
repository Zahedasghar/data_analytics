
#https://nelson-gon.github.io/12/06/2020/hex-sticker-creation-r/

library(magick)

library(dplyr)

library(hexSticker)
#I used this image for the lens.

#Basic Image Processing

img <- image_read("zahid.jpeg")
img %>% 
  image_convert("png") %>% 
  image_resize("1080 x 200")%>% 
  image_fill(color="#022047", point="+45") %>% 
  image_annotate("metRics_Zahid", size=24, location = "+07+100", color="blue") -> res

res
#Actual Sticker Creation


# wrap in plot to preview ie plot(sticker(...))
final_res<-sticker(res, package="R", p_size=30,
                   p_y = 1.5,
                   s_x=1, s_y=0.8, s_width=1.4,
                   s_height = 14,
                   filename="za_icon_2.png",h_fill="#922047",h_color = "#062047")

plot(final_res)


library(tidyverse)
library(hexSticker)
library(ggplot2)

p <- ggplot(aes(x = mpg, y = wt), data = mtcars) + geom_point()
p <- p + theme_void() + theme_transparent()
p
sticker(p, package="metRics with Zahid", p_size=12, s_x=1, s_y=.75, s_width=1.3, s_height=1,
        filename="ggplot2.png")













img <- image_read("soe_logo.jpg")
img %>% 
  image_convert("png") %>% 
  image_resize("1080 x 200")%>% 
  image_fill(color="#022047", point="+45") %>% 
  image_annotate("d?ta", size=38, location = "+07+100", color="blue") -> res

res
#Actual Sticker Creation


# wrap in plot to preview ie plot(sticker(...))
final_res<-sticker(res, package="zaR", p_size=30,
                   p_y = 1.5,
                   s_x=1, s_y=0.8, s_width=1.4,
                   s_height = 14,
                   filename="soe.png",h_fill="#922047",h_color = "#062047")
