# https://data.sandiegodata.org/dataset/sandiegodata-org-crime-victims/
# https://rdw.sandag.org/ -- this is where the hex shapefile comes from

library(tidyverse)
library(rgdal)
library(raster)
library(sf)
library(leaflet)

#x <- read.csv("http://library.metatab.org/sandiegodata.org-crime_victims-1.1.3/data/sdcrime_16_20.csv")
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



# hex map
# https://andrewpwheeler.com/2019/08/07/making-a-hexbin-map-in-ggplot/
# https://rud.is/books/30-day-map-challenge/hex-01.html
# https://www.r-bloggers.com/2020/06/creating-an-hex-map-of-france-electricity-consumption/


my_spdf <- read_polygons("C:/Users/sjack/git/SDOpenCrimeData/CMTY_HEX_GRID_060_ACRES/CMTY_HEX_GRID_060_ACRES.shp")

my_spdf <- st_read("C:/Users/sjack/git/SDOpenCrimeData/CMTY_HEX_GRID_060_ACRES/CMTY_HEX_GRID_060_ACRES.shp")


x <- readOGR("C:/Users/sjack/git/SDOpenCrimeData/Community_Plan_SD/COMMUNITY_PLAN_SD.shp")
y <- readOGR("C:/Users/sjack/git/SDOpenCrimeData/Community_Plan_CN/COMMUNITY_PLAN_CN.shp")
z <- merge(x,y)


city_spdf <- st_read("C:/Users/sjack/git/SDOpenCrimeData/Community_Plan_SD/COMMUNITY_PLAN_SD.shp")
county_spdf <- st_read("C:/Users/sjack/git/SDOpenCrimeData/Community_Plan_CN/COMMUNITY_PLAN_CN.shp")

sandiego_spdf <- rbind(city_spdf,county_spdf)

p <- ggplot() +
  geom_polygon(city_spdf)#, aes(x=X_COORD, y=Y_COORD, group = HEXAGONID))

leaflet() %>%
  addTiles() %>%
  geom_polygon(aes(data = my_spdf))

# hex maps
library(hexbin)
library(ggspatial)

ggplot(sdcrime_16_20, aes(x=intptlon, y = intptlat)) +
  stat_bin_hex() +
  scale_fill_gradient(c("white","red"), name = "Frequency")


ggplot(sdcrime_16_20, aes(x=intptlon, y = intptlat)) +
  annotation_map_tile() +
  stat_bin_hex() +
  scale_fill_gradient(c("white","red"), name = "Frequency")


# density maps

ggplot(sdcrime_16_20, aes(x=intptlon, y = intptlat)) +
  annotation_map_tile() + 
  stat_density2d(aes(fill = ..level.., # value corresponding to discretized density estimates 
                     alpha = ..level..), 
                 geom = "polygon") +  # creates the bands of different colors
  ## Configure the colors, transparency and panel
  scale_fill_gradientn(colours = c("white","red"), 
                       name = "Frequency") 


#### https://stackoverflow.com/questions/63972568/how-to-create-a-hexbin-polygon-over-a-world-map-with-a-geom-sf-object-in-r

x <- st_transform(geojson)
y <- st_make_grid(x,
                  n= c(10,10),
                  what = 'polygons',
                  square = FALSE,
                  flat_topped = TRUE) %>%
  st_as_sf() %>%
  mutate(area = st_area(.))

ggplot() + 
  geom_sf(data = y,
          aes(fill = units::drop_units(area)))
