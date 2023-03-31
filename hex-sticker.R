
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

img <- image_read("docs/images/zahid.jpeg")
img %>% 
  image_convert("png") %>% 
  image_resize("1080 x 200")%>% 
  image_fill(color="#022047", point="+45") %>% 
  image_annotate("metRics", size=38, location = "+07+100", color="gray") -> res

res
#Actual Sticker Creation


# wrap in plot to preview ie plot(sticker(...))
final_res<-sticker(res, package="Zahid Asghar", p_size=20,
                   p_y = 1.5,
                   s_x=1, s_y=0.8, s_width=1.4,
                   s_height = 14,
                   filename="za_icon_2.png",h_fill="#922047",h_color = "#062047")

plot(final_res)


library(tidyverse)
library(hexSticker)
library(ggplot2)
library(ggthemes)
p <- ggplot(aes(x = mpg, y = wt), data = mtcars) + geom_point()+geom_smooth(method = "loess",se=FALSE)
p <- p + theme_void() + theme_fivethirtyeight()

fin_st<-sticker(p, package="Data Analytics", p_size=12, s_x=1, s_y=.75, s_width=1.3, s_height=1,
        filename="ggplot2.png",h_fill="#401097",h_color = "#062047")

plot(fin_st)
img1 <- image_read("soe_logo.jpg")
img1 %>% 
  image_convert("png") %>% 
  image_resize("1080 x 200")%>% 
  image_fill(color="#022047", point="+45") %>% 
  image_annotate("Islamabad", size=38, location = "+10-10", color="white") -> res1

res1
#Actual Sticker Creation


# wrap in plot to preview ie plot(sticker(...))
final_res1<-sticker(res1, package="SOE, QAU", p_size=20,
                   p_y = 1.5,
                   s_x=1, s_y=0.8, s_width=1.0,
                   s_height = 14,
                   filename="soe.png",h_fill="#022047",h_color = "#062047")
plot(final_res1)



p1<-mpg %>%
  ggplot(aes(hwy, cty, color = class)) +
  geom_point(size = 2, alpha = 0.3) +
  geom_smooth(aes(color = NULL), se=TRUE) +
  geom_xsidedensity(
    aes(
      y    = after_stat(density),
      fill = class
    ),
    alpha    = 0.5,
    size     = 1
    ,
    position = "stack"
  ) +
  geom_ysidedensity(
    aes(
      x    = after_stat(density),
      fill = class
    ),
    alpha    = 0.5,
    size     = 1
    ,
    position = "stack"
  ) +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  labs(title = "Fuel Economy by Vehicle Type" ,
       subtitle = "ggside density",
       x = "Highway MPG", y = "City MPG") +
  theme(
    ggside.panel.scale.x = 0.4,
    ggside.panel.scale.y = 0.4
  )
p1
fin_st1<-sticker(p1, package="Data Analytics", p_size=12, s_x=1, s_y=.75, s_width=1.3, s_height=1,
                filename="ggplot_2.png",h_fill="#401097",h_color = "#062047")

plot(fin_st1)






img2 <- image_read("https://upload.wikimedia.org/wikipedia/commons/thumb/8/82/C.L_Quaid-e-Azam_University_Islamabad.jpg/1280px-C.L_Quaid-e-Azam_University_Islamabad.jpg")
img2 %>% 
  image_convert("png") %>% 
  image_resize("1080 x 600")%>% 
  image_fill(color="white", point="+75") %>% 
  image_annotate("", size=25, location = "50-50", color="blue") -> res2

res2
#Actual Sticker Creation


# wrap in plot to preview ie plot(sticker(...))
final_res2<-sticker(res2, package="Q.A.U, Islamabad", p_size=12,
                   p_y = .5,
                   s_x=1.0, s_y=1.0, s_width=1.2,
                   s_height = 14,
                   filename="qau.png",h_fill="navyblue",h_color = "lightblue")
plot(final_res2)
