library(ggplot2)
library(dplyr) # easier data wrangling 
library(viridis) # colour blind friendly palette, works in B&W also
library(Interpol.T) #  will generate a large dataset on initial load
library(lubridate) # for easy date manipulation
library(ggExtra) # because remembering ggplot theme options is beyond me
library(tidyr) 
library(gganimate)


# import multple datasets and join together before eda
pd_2022 <- read_csv("https://seshat.datasd.org/pd/pd_calls_for_service_2022_datasd.csv",
                    col_types = cols(incident_num = col_character(), 
                                     date_time = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                     day_of_week = col_integer(), address_number_primary = col_integer(), 
                                     address_dir_primary = col_character(), 
                                     address_road_primary = col_character(), 
                                     address_sfx_primary = col_character(), 
                                     address_dir_intersecting = col_character(), 
                                     address_road_intersecting = col_character(), 
                                     address_sfx_intersecting = col_character(), 
                                     call_type = col_character(), disposition = col_character(), 
                                     beat = col_integer(), priority = col_integer()))

pd_2021 <- read_csv("https://seshat.datasd.org/pd/pd_calls_for_service_2021_datasd.csv",
                    col_types = cols(incident_num = col_character(), 
                                     date_time = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                     day_of_week = col_integer(), address_number_primary = col_integer(), 
                                     address_dir_primary = col_character(), 
                                     address_road_primary = col_character(), 
                                     address_sfx_primary = col_character(), 
                                     address_dir_intersecting = col_character(), 
                                     address_road_intersecting = col_character(), 
                                     address_sfx_intersecting = col_character(), 
                                     call_type = col_character(), disposition = col_character(), 
                                     beat = col_integer(), priority = col_integer()))


pd_2020 <- read_csv("https://seshat.datasd.org/pd/pd_calls_for_service_2020_datasd.csv",
                    col_types = cols(incident_num = col_character(), 
                                     date_time = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                     day_of_week = col_integer(), address_number_primary = col_integer(), 
                                     address_dir_primary = col_character(), 
                                     address_road_primary = col_character(), 
                                     address_sfx_primary = col_character(), 
                                     address_dir_intersecting = col_character(), 
                                     address_road_intersecting = col_character(), 
                                     address_sfx_intersecting = col_character(), 
                                     call_type = col_character(), disposition = col_character(), 
                                     beat = col_integer(), priority = col_integer()))

pd_2019 <- read_csv("https://seshat.datasd.org/pd/pd_calls_for_service_2019_datasd.csv", 
                    col_types = cols(incident_num = col_character(), 
                                     date_time = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                     day_of_week = col_integer(), address_number_primary = col_integer(), 
                                     address_dir_primary = col_character(), 
                                     address_road_primary = col_character(), 
                                     address_sfx_primary = col_character(), 
                                     address_dir_intersecting = col_character(), 
                                     address_road_intersecting = col_character(), 
                                     address_sfx_intersecting = col_character(), 
                                     call_type = col_character(), disposition = col_character(), 
                                     beat = col_integer(), priority = col_integer()))

pd_2018 <- read_csv("https://seshat.datasd.org/pd/pd_calls_for_service_2018_datasd.csv", 
                    col_types = cols(incident_num = col_character(), 
                                     date_time = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                     day_of_week = col_integer(), address_number_primary = col_integer(), 
                                     address_dir_primary = col_character(), 
                                     address_road_primary = col_character(), 
                                     address_sfx_primary = col_character(), 
                                     address_dir_intersecting = col_character(), 
                                     address_road_intersecting = col_character(), 
                                     address_sfx_intersecting = col_character(), 
                                     call_type = col_character(), disposition = col_character(), 
                                     beat = col_integer(), priority = col_integer()))
pd_2017 <- read_csv("https://seshat.datasd.org/pd/pd_calls_for_service_2017_datasd_v1.csv",
                    col_types = cols(incident_num = col_character(), 
                                     date_time = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                     day_of_week = col_integer(), address_number_primary = col_integer(), 
                                     address_dir_primary = col_character(), 
                                     address_road_primary = col_character(), 
                                     address_sfx_primary = col_character(), 
                                     address_dir_intersecting = col_character(), 
                                     address_road_intersecting = col_character(), 
                                     address_sfx_intersecting = col_character(), 
                                     call_type = col_character(), disposition = col_character(), 
                                     beat = col_integer(), priority = col_integer()))

pd_2016 <- read_csv("https://seshat.datasd.org/pd/pd_calls_for_service_2016_datasd_v1.csv",
                    col_types = cols(incident_num = col_character(), 
                                     date_time = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                     day_of_week = col_integer(), address_number_primary = col_integer(), 
                                     address_dir_primary = col_character(), 
                                     address_road_primary = col_character(), 
                                     address_sfx_primary = col_character(), 
                                     address_dir_intersecting = col_character(), 
                                     address_road_intersecting = col_character(), 
                                     address_sfx_intersecting = col_character(), 
                                     call_type = col_character(), disposition = col_character(), 
                                     beat = col_integer(), priority = col_integer()))

pd_2015 <- read_csv("https://seshat.datasd.org/pd/pd_calls_for_service_2015_datasd_v1.csv",
                    col_types = cols(incident_num = col_character(), 
                                     date_time = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                     day_of_week = col_integer(), address_number_primary = col_integer(), 
                                     address_dir_primary = col_character(), 
                                     address_road_primary = col_character(), 
                                     address_sfx_primary = col_character(), 
                                     address_dir_intersecting = col_character(), 
                                     address_road_intersecting = col_character(), 
                                     address_sfx_intersecting = col_character(), 
                                     call_type = col_character(), disposition = col_character(), 
                                     beat = col_integer(), priority = col_integer()))



pd <- bind_rows(pd_2018,pd_2019,pd_2020,pd_2021, pd_2022)


# in order to make things more legible, lets pull down and join the call_type definitions
call_type <- read_csv("http://seshat.datasd.org/pd/pd_cfs_calltypes_datasd.csv")
pd <- merge(pd, call_type[,1:2], by = "call_type")

# also lets pull down and join disposition information
disposition <- read_csv("http://seshat.datasd.org/pd/pd_dispo_codes_datasd.csv")
# rename column for merge 
colnames(disposition) <- c("disposition", "description")
pd <- merge(pd, disposition, by = "disposition")

# also let's pull down and join beat definitions for legibility
beat <- read_csv("https://seshat.datasd.org/pd/pd_beat_codes_list_datasd.csv")

pd <- merge(pd, beat, by = "beat")


pd$hour <- as.factor(hour(pd$date_time))
pd$day <- as.factor(day(pd$date_time))
pd$month <- as.factor(month(pd$date_time))
pd$year <- as.factor(year(pd$date_time))

hourlyTotals <- pd %>%
  group_by(hour,day,month, year) %>%
  summarise(hrTotal=n()) 

# heatmap example
hourlyTotals %>%
filter(year=='2022') %>%
ggplot(., aes(day, hour, fill = hrTotal)) +
  geom_tile(color = "white",size = 0.1) + 
  scale_fill_viridis(name = "Hourly Calls For Service", option = "C") +
  facet_grid(year~month, scales = 'free_x') +
  theme_minimal(base_size = 8) +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14, hjust = 0),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size = 6),
        strip.background = element_rect(colour = "white"),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 7),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6)) +
  removeGrid()

# daily by neighborhood
neighborhoodTotals <- pd %>%
  group_by(day,month,year,neighborhood) %>%
  summarise(neighborhoodTotal=n())

neighborhoodTotals$logTotal <- log(neighborhoodTotals$neighborhoodTotal)


neighborhoodTotals %>%
filter(year=='2022') %>%
  ggplot(., aes(day, neighborhood, fill = neighborhoodTotal)) +
  geom_tile(color = "white",size = 0.1) + 
  scale_fill_viridis(name = "Daily Service Calls by Neighborhood", option = "C") +
  facet_grid(year~month, scales = 'free_x') +
  theme_minimal(base_size = 8) +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14, hjust = 0),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size = 6),
        strip.background = element_rect(colour = "white"),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 7),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6)) +
  removeGrid()

