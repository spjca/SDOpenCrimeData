# paste address together

library(tidyverse)

x <- pd_2020 %>%
      unite("full_address", c(address_number_primary, address_dir_primary, address_road_primary, address_sfx_primary, ", San Diego, CA"), sep = " ", na.rm = TRUE) %>%
      unite("intersecting_address", c(address_dir_intersecting, address_road_intersecting, address_sfx_intersecting, "San Diego, CA"), sep = " ", na.rm = TRUE)

cols <- c('address_number_primary', 'address_dir_primary', 'address_road_primary', 'address_sfx_primary')

pd_2020$x <- apply( pd_2020[ , cols ] , 1 , paste , collapse = " " )
