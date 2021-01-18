
library(dplyr)
library(leaflet)
library(geojsonio)
library(jsonlite)
library(raster)


geojson <- geojsonio::geojson_read("pd_beats_datasd.geojson", what = "sp")

geojson$style = list(
  weight = 1,
  color = "#555555",
  opacity = 1,
  fillOpacity = 0.8
)


beat_names <- sapply(geojson$features, function(feat) {
  feat$properties$name
})


beats <- sapply(geojson$features, function(feat) {
  feat$properties$beat
})

leaflet(geojson) %>%
  addTiles() %>%
  #addPolygons()
  addPolygons(stroke = TRUE, 
              smoothFactor = 0.3, 
              #fillOpacity = 1,
              #fillColor = beat,#~pal(log10(pop)),
              label = ~paste0(name),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) #, ": ", formatC(pop, big.mark = ",")))

# using the eda.R script, create the crime_beat_sums dataframe to join
# with the geojson object

m <- sp::merge(geojson,crime_beat_sums, by = 'beat')

# create variable with lable information pulled from geojson data
mytext <- paste(
            "Beat: ", m@data$name, "<br/>",
            "Total Calls 2020: ", m@data$ct, "<br/>", sep = " "
        ) %>%
        lapply(htmltools::HTML)

              
sd_crime_map <- leaflet(m) %>%
  addTiles() %>%
  addPolygons(stroke = TRUE, 
              smoothFactor = 0.3, 
              label = mytext,
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))

htmlwidgets::saveWidget(sd_crime_map, "sd_crime_map.html")


spplot(m,"ct")


#############
library(ggplot2)
library(ggmap)
library(gganimate)
library(sf)
library(geojsonsf)
library(rnaturalearth)
library(rnaturalearthdata)

geojson <- geojson_sf("pd_beats_datasd.geojson")

#n <- sp::merge(geojson,crime_beat_daily_ts, by = 'beat')
n <- sp::merge(geojson,crime_beat_sums, by = 'beat')

ggplot(data = n) +
  geom_sf(aes(fill=ct)) +
  scale_fill_gradient(name = 'Total', trans = 'log', breaks = c(1,max(p$value))) +
  theme_bw() + theme(legend.position="bottom", panel.border = element_blank())


# this is more or less working for a single map, let's try an animation
p <- sp::merge(geojson,crime_beat_daily_ts, by = 'beat')

p1 <- ggplot(data = p) +
        geom_sf(aes(fill = value)) +
        # can't use line below to scale due to 0's in some beats some days
        scale_fill_gradient(name = 'Total', trans = 'log', breaks = c(0,max(p$value))) +
        theme_bw() + theme(legend.position="bottom", panel.border = element_blank())

p2 <- p1 + transition_time(date) + labs(title = "Date: {frame_time}")

animate(p2, end_pause = 30)
