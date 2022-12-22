# hex plot 
# https://www.r-bloggers.com/2020/06/creating-an-hex-map-of-france-electricity-consumption/

library(tidyverse)  
library(viridis)   
library(ggplot2)   
library(stringr)


sdcrime_16_20 <- read_csv("sdcrime_16_20.csv", 
                          col_types = cols(
                            pk = col_integer(), 
                            activitynumber = col_character(), 
                            activitydate = col_datetime(format = "%Y-%m-%d %H:%M:00"), 
                            year = col_integer(), 
                            agency = col_character(), 
                            violationsection = col_character(), 
                            violationtype = col_character(), 
                            chargedescription = col_character(), 
                            chargelevel = col_character(), 
                            codeucr = col_character(), 
                            crimecategory = col_character(), 
                            personrole = col_character(), 
                            race = col_character(), 
                            age = col_integer(), 
                            sex = col_character(), 
                            zipcode = col_integer(), 
                            censusblock = col_character()))


ggplot(sdcrime_16_20, 
       aes(x=intptlat,
           y=intptlon)
      ) +
  stat_summary_hex(bins = 40,
                   aes(z=city),
                   fun = "sum",
                   colour="grey") +
  theme_void()


# https://www.robertdinterman.com/2016-12-22-hexbin-animation/
library(gganimate)  
library(viridis)

sdcrime_hex <- sdcrime_16_20 %>%
  filter(chargelevel=='FELONY') %>%
  ggplot( aes(x=intptlon, y=intptlat)) + 
  geom_hex(bins=59) +
  #ggplot2::annotate("text", x = -27, y = 72, label="Where people tweet about #Surf", colour = "black", size=5, alpha=1, hjust=0) +
  #ggplot2::annotate("segment", x = -27, xend = 10, y = 70, yend = 70, colour = "black", size=0.2, alpha=1) +
  theme_void() +
  #xlim(-30, 70) +
  #ylim(24, 72) +
  scale_fill_viridis(
    trans = "log",
    breaks = c(1,100,500,1000,2500,5000),
    #name="Tweet # recorded in 8 months",
    guide = guide_legend( keyheight = unit(2.5, units = "mm"), keywidth=unit(10, units = "mm"), label.position = "top", title.position = 'top', nrow=1)
  )  +
  ggtitle( "" ) +
  theme(
    legend.position = c(0.8, 0.09),
    legend.title=element_text(color="black", size=8),
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 13, hjust=0.1, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  ) 

