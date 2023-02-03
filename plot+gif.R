library(gt)
library(gtExtras)
library(tidyverse)
sector<-c("Agriculture"  ,"Industry", "Services", "GDP")
growth<-c(-3.5,-4,1,-1.0)
df1<-bind_cols(sector,growth)
colnames(df1)<-c(" Sector","growth rate % ")
df1|>gt()|>gt_theme_dark()|>tab_header("Pakistan projected economic growth: grim outlook")|>
  tab_source_note("Hafiz Pasha, brecorder")
sec2<-c("Consumption Expenditure
(public + private)","Investment  ","Change in Stocks","Exports of Goods and Services",
        "Imports of Goods and Services","Indirect Taxes minus Subsidies ","GDP at factor cost")  
        
gr2<-c( -3.5,  -15.0,    -1.0,-2.5,-18.0,-5.0,-1.0)
df2<-bind_cols(sec2,gr2)
colnames(df2)<-c("sector", "growth rate %")
df2|>gt()|>gt_theme_pff()|>tab_caption("Pakistan projected economic growth: grim outlook")|>
  tab_source_note("Hafiz Pasha, brecorder")

## Haemorrhaging
sector2<-c("power (2-decades)","gas (2-decades)","commodity operations (2-decades)",
           "PIA per year)",
           "PSMill per year","textile cartel(subsidies couple of yrs)", "fertilizer cartel(per year)")
Haemorrhaging<-c(2500,1500,800, 67,134,1000,150)
hamo<-bind_cols(sector2,Haemorrhaging)
colnames(hamo)<-c("sector","Rs_in_bill")
hamo|>gt()|>
  tab_header(title = 'Haemorrhaging Pak Economy',
             subtitle = 'Exact sources of bleeding are known, but surgeons are missing') |> 
  tab_footnote(footnote = 'Public Sector Enterprizes (PSEs) debt now stands at Rs2 trillion'
  )|>gt_theme_pff()|>tab_source_note("Source : After the IMF by Dr. Farrukh Saleem")

trans<-c("United States","Multilaterals", "China", "International bonds", "IMF outstanding with 23 prog")
amount<-c(78.3,60,37,10,7.8)  
transf<-bind_cols(trans,amount)
colnames(transf)<-c("aid_provider", "Amount_bill_USD")
transf|>gt()|> tab_header(title = 'Blood transfusion: Pak Haemorrhaging Economy',
                          subtitle = 'No more blood available and urgent need to have surgery to stop haemorrhaging') |> 
  tab_footnote(footnote = 'Are there competent surgeons available?'
  )|>gt_theme_pff()|>tab_source_note("Source : After the IMF by Dr. Farrukh Saleem")

transf|>mutate(aid_provider = fct_reorder(aid_provider, Amount_bill_USD))|>
ggplot(aes(x=aid_provider,y=Amount_bill_USD))+geom_col(fill = "#6667AB") +
  #geom_bar(stat="identity")+
  #coord_flip()+
  geom_text(aes(label = Amount_bill_USD), vjust = 2, colour = "white")+
  theme_minimal() +
  theme(axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        text = element_text(family = "Georgia"),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 20, margin = margin(b = 10), hjust = 0),
        plot.subtitle = element_text(size = 12, color = "darkslategrey", margin = margin(b = 25, l = -25)),
        plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey70", hjust = 0))+
    labs(title = "Billions of USD received in history",
       caption = "Source : Dr Farrukh Saleem, chart by: Zahid Asghar")


library(tidyverse)
horizontal_bars<-hamo|>mutate(sector = fct_reorder(sector, Rs_in_bill))|>
  ggplot(aes(x=sector,y=Rs_in_bill))+geom_col(fill = "#6667AB") +
  #geom_bar(stat="identity")+
  coord_flip()+
  geom_text(aes(label = Rs_in_bill), hjust = -.01, colour = "black")+
  theme_minimal()+ labs(
    y = element_blank(), 
    x = element_blank(),
    title = 'Haemorrhaging Pakistani Economy of trillions of Rs',
    cation=' Farrukh Saleem: After the IMF ' 
  )+theme_minimal()
horizontal_bars  


larger_text <- horizontal_bars +
  theme_grey(base_size = 14) +
  theme(plot.title = element_text(size = rel(1.1)))
larger_text


horizontal_bars_no_spacing <- larger_text +
  scale_x_continuous(expand = expansion(mult = c(0, 0.01)))
horizontal_bars_no_spacing


no_y_grid_plot <- horizontal_bars_no_spacing +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
  )
no_y_grid_plot

p<-larger_text+
  theme_minimal()
p
library(magick)
gif<-image_read("https://www.animatedimages.org/data/media/421/animated-blood-image-0003.gif")
plot <- image_graph(width = 1000, height = 500, res = 100)
p
dev.off()
frames <- image_composite(plot, gif, offset = "+500-100")
frames
animation <- image_animate(image_join(frames), optimize = TRUE)
animation <- image_quantize(animation,
                            max = 20,
                            colorspace = "rgb")

animation


image_write(animation, "C:D:\\RepTemplates\\data_analytics\\bleeding_econ.gif")

animation <- image_animate(image_join(frames), optimize = TRUE, loop = 1)

animation <- image_quantize(animation,
                            max = 20,
                            colorspace = "rgb")
length(gif)
gif<-gif|>image_scale("300")

