# https://www.r-graph-gallery.com/183-choropleth-map-with-leaflet.html


library(tidyverse)
library(readr)
library(lubridate)
library(data.table)
library(xts)
library(anomalize)
library(tibbletime)
library(timetk)

# import multple datasets and join together before eda
pd_2020 <- read_csv("https://seshat.datasd.org/pd/pd_calls_for_service_2020_datasd.csv",
#pd_2020 <- read_csv("pd_calls_for_service_2020_datasd.csv", 
            #col_types = cols(date_time = col_datetime(format = "%Y-%m-%d %H:%M:%S")))
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

#pd <- bind_rows(pd_2015, pd_2016, pd_2017, pd_2018,pd_2019,pd_2020)
pd <- bind_rows(pd_2018,pd_2019,pd_2020)


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

# create column for hour
pd_2020$hour <- as.POSIXlt(pd_2020$date_time)$hour
pd$hour <- as.POSIXlt(pd$date_time)$hour


# count of calls on each day of week
pd_2020 %>%
  group_by(day_of_week) %>%
  count()

# higher in the week, lower on the weekends

# let's take a look at the counts by month and day of week
calls_dow_monthly <- pd_2020 %>%
  group_by(months(date_time),day_of_week) %>%
  count()  %>%
  spread(`months(date_time)`, n)

# let's also reorder the columns so they make sense
col_order <- c('day_of_week','January','February','March','April',
               'May','June','July','August','September',
               'October','November','December')
calls_dow_monthly <- calls_dow_monthly[,col_order]
rm(col_order)

# let's add a column with the average value each day of week across months
calls_dow_monthly$avg <- calls_dow_monthly %>%
                          rowMeans()

# and standard deviation column 
calls_dow_monthly$sd <- apply(calls_dow_monthly[2:8], 1, sd)

# let's see what it looks like subtracting out the average for each row
calls_dow_less_avg <- pd_2020 %>%
  group_by(months(date_time),day_of_week) %>%
  count()  %>%
  spread(`months(date_time)`, n)

# let's also reorder the columns so they make sense
col_order <- c('day_of_week','January','February','March','April',
               'May','June','July','August','September',
               'October','November','December')
calls_dow_less_avg <- calls_dow_less_avg[,col_order]
rm(col_order)

calls_dow_less_avg <- calls_dow_less_avg[,2:13] - rowMeans(calls_dow_less_avg[,2:13])
                      

# let's try getting more granular
# group by day of week/hour of day
calls_dow_hourly <- pd_2020 %>%
                    group_by(months(date_time),day_of_week,hour) %>%
                    count() 

calls_dow_hourly <- calls_dow_hourly %>%
                    unite( "dow_hour", day_of_week:hour)


calls_dow_hourly <- calls_dow_hourly %>%
  spread(`months(date_time)`, n)

col_order <- c('dow_hour','January','February','March','April',
               'May','June','July','August','September',
               'October','November','December')

calls_dow_hourly <- calls_dow_hourly[,col_order]

rm(col_order)

# let's try calls each hour each month
calls_month_hourly <- pd_2020 %>%
  group_by(months(date_time),hour) %>%
  count()  %>%
  spread(`months(date_time)`, n)

col_order <- c('hour','January','February','March','April',
               'May','June','July','August','September',
               'October','November','December')

calls_month_hourly <- calls_month_hourly[,col_order]
rm(col_order)


# let's try daily counts then time series decomposition and anomaly detection
#calls_daily_ts <- data.table(table(data.table(format(pd_2020$date_time, format = "%Y-%m-%d"))))
#calls_daily_ts <- data.table(table(data.table(format(pd$date_time, format = "%Y-%m-%d"))))
calls_daily_ts <- data.table(table(data.table(format(pd_2020$date_time, format = "%Y-%m-%d"))))
calls_daily_ts$V1 <- as.Date(calls_daily_ts$V1)

ggplot(calls_daily_ts, aes(x = V1,y = N)) + geom_line()

# plot anomalies 
as_tbl_time(calls_daily_ts, V1) %>%
  time_decompose(N) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  plot_anomalies()

# decompose into seasonality, trend, remainder
as_tbl_time(calls_daily_ts, V1) %>%
  time_decompose(N, 
                 method = "stl", 
                 frequency = "auto", 
                 trend = "2 weeks") %>%
  anomalize(remainder, 
            method = "gesd", 
            alpha = 0.02, 
            max_anoms = 0.2) %>%
  # Plot Anomaly Decomposition with title
  plot_anomaly_decomposition() +
  ggtitle("San Diego Police Calls for Service: Anomaly Decomposition")
                 
# let's try hourly counts then time series decomposition and anomaly detection
#calls_hourly_ts <- data.table(table(data.table(format(pd_2020$date_time, format = "%Y-%m-%d %H:00:00"))))
calls_hourly_ts <- data.table(table(data.table(format(pd$date_time, format = "%Y-%m-%d %H:00:00"))))
calls_hourly_ts$V1 <- as_datetime(calls_hourly_ts$V1)

ggplot(calls_hourly_ts, aes(x = V1,y = N)) + geom_line()

# plot anomalies
as_tbl_time(calls_hourly_ts, V1) %>%
  time_decompose(N) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  plot_anomalies()

# plot decomposition
as_tbl_time(calls_hourly_ts, V1) %>%
  time_decompose(N, method = "stl", frequency = "auto", trend = "auto") %>%
  anomalize(remainder, method = "iqr", alpha = 0.05, max_anoms = 0.2) %>%
  
# Plot Anomaly Decomposition
plot_anomaly_decomposition() +
ggtitle("San Diego Police Calls for Service: Anomaly Decomposition")


# now let's see how many service calls of each priority occur each hour
x <- pd
x$date_time <- format(pd_2020$date_time, format = "%Y-%m-%d %H:00:00")
x$priority <- sub("^",'priority_',x$priority)

calls_hourly_priority_ts <- x %>%
  group_by(date_time,priority) %>%
  count(name = 'ct') 

# %>%
#   pivot_wider(names_from = priority, 
#               values_from = ct,
#               values_fill = 0,
#               names_sort = TRUE)




ggplot(calls_hourly_priority_ts, aes(x = date_time, y = ct, group = priority)) +
  geom_line(aes(color = priority), size = 1) +
  facet_wrap(~ priority, scales = 'free_y', ncol = 1)


# now let's see how many service calls of each priority occur each day
pd_cleaned <- pd_2020[!(pd_2020$priority=='5' | pd_2020$priority == '7' | pd_2020$priority == '8' | is.na(pd_2020$priority)),]

x <- pd_cleaned
x$date_time <- format(pd_cleaned$date_time, format = "%Y-%m-%d")
x$priority <- sub("^",'priority_',x$priority)

calls_daily_priority_ts <- x %>%
  group_by(date_time,priority) %>%
  count(name = 'ct')

calls_daily_priority_ts$date_time <- as.Date(calls_daily_priority_ts$date_time)

t <- ggplot(calls_daily_priority_ts, aes(x = date_time, y = ct, group = priority)) +
  geom_line(aes(color = priority), size = 1) +
  geom_point() +
  #geom_segment(aes(xend = 31, yend = ct), linetype = 2, colour = 'grey') +
  #geom_text(aes(x = 31.1, label = ct), hjust = 0) + 
  facet_wrap(~ priority, scales = 'free_y', ncol = 1) #+   stat_smooth(method = "loess")

# checking something from gganimate
t1 <- t + geom_point() + transition_reveal(date_time)

animate(t1, end_pause = 30)

# okay, so we learned a few things here:
# - prior to 2016 all calls had the same priority
# - prior to ~2018 the priority dispatch was significantly different
# - priority 5-8 are not frequently used and may be able to be dropped

pd_cleaned <- pd[!(pd$priority=='5' | pd$priority == '7' | pd$priority == '8' | is.na(pd$priority)),]

x <- pd_cleaned
x$date_time <- as.Date(format(pd_cleaned$date_time, format = "%Y-%m-%d"))
x$priority <- sub("^",'priority_',x$priority)

calls_daily_priority_ts <- x %>%
  group_by(date_time,priority) %>%
  count(name = 'ct')

ggplot(calls_daily_priority_ts, aes(x = date_time, y = ct, group = priority)) +
  geom_line(aes(color = priority), size = 1) +
  facet_wrap(~ priority, scales = 'free_y', ncol = 1) +
  stat_smooth(method = "loess") +
  scale_x_date(date_labels =  "%b %Y") 


# this makes me wonder what types of crimes are being dispatched at each priority

crime_priorty <- pd_cleaned %>% 
                  group_by(priority,description.x) %>% 
                  count(name = 'ct') %>%
                  ungroup(priority,description.x) %>%
                  arrange(priority, desc(ct), by_group = TRUE)

# and same thing but also by beat
crime_beat_priorty <- pd_cleaned %>% 
  group_by(neighborhood,priority,description.x) %>% 
  count(name = 'ct') %>%
  ungroup(neighborhood,priority,description.x) %>%
  arrange(neighborhood, priority, desc(ct), by_group = TRUE)



# working with the geojson beats data, let's aggregate crimes in each beat
# for the map display

crime_beat_sums <- pd_2020 %>% 
  group_by(beat) %>% 
  count(name = 'ct') %>%
  ungroup(beat) %>%
  arrange(beat, desc(ct), by_group = TRUE)


### crime beat ts
#crime_beat_daily_ts <- data.table(table(format(pd$date_time,format = "%Y-%m-%d"), pd$beat)) %>% 
crime_beat_daily_ts <- data.table(table(format(pd_2020$date_time,format = "%Y-%m-%d"), pd_2020$beat)) %>% 
  set_names(c("date","beat","value")) 
crime_beat_daily_ts$date <- as_datetime(crime_beat_daily_ts$date)
crime_beat_daily_ts$beat <- as.factor(crime_beat_daily_ts$beat)




################################ timetk package 
calls_hourly_ts <- data.table(table(data.table(format(pd$date_time, format = "%Y-%m-%d %H:00:00")))) %>%
  set_names(c("date", "value")) 
calls_hourly_ts$V1 <- as_datetime(calls_hourly_ts$date)

calls_daily_ts <- data.table(table(data.table(format(pd$date_time, format = "%Y-%m-%d")))) %>%
  set_names(c("date", "value")) 
#calls_daily_ts <- data.table(table(data.table(format(pd_2020$date_time, format = "%Y-%m-%d"))))
calls_daily_ts$date <- as.Date(calls_daily_ts$date)


calls_daily_ts %>%
  plot_time_series(date, value, .color_var = week(date))#, interactive= FALSE, .color_lab = "Week")


calls_daily_ts %>%
  plot_seasonal_diagnostics(date, value, .interactive = FALSE)


calls_daily_ts %>%
  plot_acf_diagnostics(date,value,.lags = "1 week", .interactive = FALSE)


#### https://business-science.github.io/timetk/articles/TK03_Forecasting_Using_Time_Series_Signature.html

library(tidymodels)
library(modeltime)
library(tidyverse)

split <- calls_daily_ts %>%
  time_series_split(assess = "3 months", cumulative = TRUE)

split %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, value) #, .interactive = interactive)


recipe_spec_timeseries <- recipe(value ~ ., data = training(split)) %>%
  step_timeseries_signature(date) 

bake(prep(recipe_spec_timeseries), new_data = training(split)) # check outputs

recipe_spec_final <- recipe_spec_timeseries %>%
  step_fourier(date, period = 365, K = 5) %>%
  step_rm(date) %>%
  step_rm(contains("iso"), contains("minute"), contains("hour"),
          contains("am.pm"), contains("xts")) %>%
  step_normalize(contains("index.num"), date_year) %>%
  step_dummy(contains("lbl"), one_hot = TRUE) 

juice(prep(recipe_spec_final))


model_spec_lm <- linear_reg(mode = "regression") %>%
  set_engine("lm")


workflow_lm <- workflow() %>%
  add_recipe(recipe_spec_final) %>%
  add_model(model_spec_lm)

workflow_lm

workflow_fit_lm <- workflow_lm %>% fit(data = training(split))

model_table <- modeltime_table(
  workflow_fit_lm
) 

model_table


calibration_table <- model_table %>%
  modeltime_calibrate(testing(split))

calibration_table

calibration_table %>%
  modeltime_forecast(actual_data = calls_daily_ts) %>%
  plot_modeltime_forecast()#.interactive = interactive)


calibration_table %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy()


calibration_table %>%
  modeltime_refit(calls_daily_ts) %>%
  modeltime_forecast(h = "12 months", actual_data = calls_daily_ts) %>%
  plot_modeltime_forecast()


####### https://business-science.github.io/timetk/articles/TK05_Plotting_Seasonality_and_Correlation.html

calls_daily_ts %>%
  select(date, value) %>%
  #group_by(id) %>%
  plot_acf_diagnostics(
    date, value,        # ACF & PACF
    #.ccf_vars    = c(Temperature, Fuel_Price),   # CCFs
    .lags        = "3 months"#,    # 3 months of weekly lags
    #.interactive = interactive
  )


calls_hourly_ts %>%
  plot_seasonal_diagnostics(date, value, interactive = FALSE)


