# https://stackoverflow.com/questions/59119667/animated-geographical-maps-in-r-with-gganimate

library(tidyverse)
library(rnaturalearth) 
library(rnaturalearthdata)
library(gganimate) # also needs transformr
#library(rgdal)

## Do all previous stuff
set.seed(1234)

ww <- ne_countries(scale = "medium", returnclass = "sf")

ll <- ww$name %>% length

val <- sample(c("a","b","c","d"), ll, replace=T)
bb <- ne_download(type = "wgs84_bounding_box", category = "physical",
                  returnclass = "sf")
ww <- ww %>% mutate(value=val)


newdf <- lapply(seq_len(5), function(i) {
  new <- ww
  new$group <- seq_len(nrow(new))
  new$value <- sample(letters[1:4], nrow(new), replace = TRUE)
  new$time <- i
  new
})
newdf <- do.call(rbind, newdf)

gpl1 <- ggplot(data = newdf) +
  geom_sf(aes(fill=value, group = group),  col = "black", lwd = 0.3 )+
  xlab(NULL) + ylab(NULL) +
  ggtitle("World Export of Merchandise", subtitle = "{frame_time}")+
  geom_sf(data = bb, col = "grey", fill = "transparent") +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(colour = "grey"),
        legend.position="top",
        plot.title = element_text(lineheight=.8, size=24, face="bold",
                                  vjust=1),
        legend.text = element_text(vjust=.4,lineheight=1,size = 14),
        legend.title = element_text(vjust=1,lineheight=1, size=14,
                                    face="bold" )) +
  transition_time(time)
# coord_sf(crs = "+proj=eqearth +wktext") # couldn't get this coord to work

ani <- animate(gpl1)
