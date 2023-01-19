
#https://nelson-gon.github.io/12/06/2020/hex-sticker-creation-r/
#install.packages("magick")
library(magick)
## Linking to ImageMagick 6.9.9.14
## Enabled features: cairo, freetype, fftw, ghostscript, lcms, pango, rsvg, webp
## Disabled features: fontconfig, x11
library(dplyr)
## 
## Attaching package: 'dplyr'
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
#install.packages("hexSticker")
library(hexSticker)
#I used this image for the lens.

#Basic Image Processing

img <- image_read("zahid.png")
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
                   filename="za_icon_2.png",h_fill="#922047",h_color = "#062047")

plot(final_res)


library(tidyverse)
library(hexSticker)
library(ggplot2)

p <- ggplot(aes(x = mpg, y = wt), data = mtcars) + geom_point()
p <- p + theme_void() + theme_transparent()
p
sticker(p, package="hexSticker", p_size=8, s_x=1, s_y=.75, s_width=1.3, s_height=1,
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
