library(ggthemes)
library(tidyverse)
library(gganimate)
# install.packages("WDI")
# install.packages("RJSONIO")
library(RJSONIO)
library(WDI)

theme_set(theme_minimal())
#Let's see if we can find a code for export?

WDIsearch(string='Export ', field='name', cache=NULL)

#Ahha, seems like "NE.EXP.GNFS.CD" (Export of Goods and Services) will do it...

#NE.EXP.GNFS.CD

WDI(
  country = "all",
  indicator = c("NE.EXP.GNFS.CD"),
  start = 1980,
  end = 2021
) |> filter(country == 'Pakistan' |
              country == 'Bangladesh') |> mutate(export = NE.EXP.GNFS.CD / 1000000) |>
  ggplot() + aes(x = year, y = export, color = country) + geom_line() +
  labs(
    title = "Export of goods and service in million USD",
    x = "year",
    y = "current USD",
    caption = "Source:WDI"
  ) + theme_minimal() 
+ transition_reveal(year)




df_exp <-
  WDI(
    country = "all",
    indicator = c("NE.EXP.GNFS.CD"),
    start = 1980,
    end = 2021
  )
df_exp |> glimpse()
# saveRDS(df_exp,file = "df_export.rds")

df_exp <- readRDS("df_export.rds")
df_exp |> filter(country=="Pakistan") |> View()

## We can assign data name for BD and PK if we like

Bd_Pk <-
  df_exp  |>  filter(country == 'Pakistan' | country == 'Bangladesh') |> mutate(export=NE.EXP.GNFS.CD/1000000)

ggplot(Bd_Pk, aes(x = year, y = export, color = country)) + geom_line() +
  labs(
    title = "Export of goods and service in million USD",
    x = "year",
    y = "current USD",
    caption = "Source:WDI"
  ) + theme_minimal() + transition_reveal(year)

## Animated one
an <-
  ggplot(Bd_Pk, aes(x = year, y = export, color = country)) + geom_line(size=1) +
  labs(
    title = "Export of goods \n and service in \n current USD",
    x = "year",
    y = "current USD",
    caption = "Source:WDI"
  )

p <- an+transition_reveal(year)

  anim_save("export.gif", animation = animate(p), renderer = gifski_renderer())


# Save the animation as a GIF
#anim_save("export.gif", animation = animate(p), renderer = gifski_renderer())

# Save the animation as an MP4
anim_save("export.mp4", animation = animate(p), renderer = av_renderer())

## One line code
df_exp |> filter(country%in%c('Pakistan', 'India','Bangladesh')) |> 
  mutate(export=NE.EXP.GNFS.CD/1000000) |> 
                   ggplot()+aes(x=year,y=export, group=country) +
  geom_line(size=1.0)



# g=ggplot(df_exp)+geom_line(sel_wdi=subset(df_exp,country%in%c('Pakistan', 'India','Bangladesh')),aes(x=year,y=NE.EXP.GNFS.CD),col='red')
# g
#So what's causing the drop Export of Goods and Services? One way of exploring
#this problem is to look at the Export of Goods and Services figures for other
#countries with known problems over a particular period to see if their Export
#of Goods and Services figures have a similar signature over that particular
#period. So for example, let's bring in in sel_wdi for Kenyan life expectancy -
#does the Aids epidemic that hit that country have a similar signarture
#effect?
library(scales)
df_exp |> filter(country%in%c('Pakistan', 'India', 'Bangladesh')) |>
  mutate(export=NE.EXP.GNFS.CD/1000000) |>  
  ggplot()+aes(x=year,y=export, group=country) +
  geom_line(aes(color=country),size=1) + scale_y_log10()

  #transition_reveal(year)


WDI(
  country = "all",
  indicator = c("NE.EXP.GNFS.CD"),
  start = 1980,
  end = 2021
)|> filter(country%in%c('Pakistan', '', 'Bangladesh')) |> 
  ggplot()+aes(x=year,y=NE.EXP.GNFS.CD, group=country) +
  geom_line(aes(color=country))+ transition_reveal(year) +
  labs(title="Export ")





sel_wdi <- WDI(
  indicator = c(
    "EG.ELC.ACCS.ZS",
    # access to electricity
    "BN.CAB.XOKA.GD.ZS",
    # current account balance
    "IC.BUS.DFRN.XQ",
    # ease of doing business
    "FP.CPI.TOTL.ZG",
    # CPI
    "FR.INR.LNDP",# interest rate spread
  ),
  
  start = 1960,
  end = 2021
) |> as_tibble() 

saveRDS(sel_wdi,file="sel_wdi.rds")
sel_wdi <- readRDS("sel_wdi.rds")
sel_wdi |> glimpse()
sel_wdi<-sel_wdi |>
  rename(elecperpop = 5,
         cab = 6,
         edb = 7,
         cpi = 8,
         ratespread = 9) 

library(stevemisc)

sel_wdi |>
  filter(country == "Pakistan") |>
  mutate(cpiprop = cpi/100) |> # going somewhere with this...
  ggplot()+aes(year, cpiprop)+ 
  theme_steve_web() + post_bg() +
  geom_bar(stat="identity", alpha=.8, fill="#619cff", color="black") +
  scale_x_continuous(breaks = seq(1960, 2021, by = 10)) +
  # Below is why I like proportions
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y = "Consumer Price Index (Annual %)",
       caption = "sel_wdi: International Monetary Fund, via {WDI}",
       title = "The Consumer Price Index (Annual %) in Pakistan, 1960-2020",
       subtitle = "International events,debt and currency devaluations will account for the spikes you see.")


sel_wdi |>
  filter(country == "Pakistan") |>
  #mutate(cpiprop = cpi/100) |> # going somewhere with this...
  ggplot()+aes(year, cab)+ 
  theme_steve_web() + post_bg() +
  geom_bar(stat="identity", alpha=.8, fill="#619cff", color="black") +
  scale_x_continuous(breaks = seq(1960, 2021, by = 10)) +
  # Below is why I like proportions
  #scale_y_continuous(labels = scales::percent) +
  labs(x = "", y = "Current Account Balance",
       caption = "sel_wdi: International Monetary Fund, via {WDI}",
       title = "Current Account Balance in Pakistan, 1960-2021",
       subtitle = "Pakistan current account balance has hardly been favorable in its history.")



