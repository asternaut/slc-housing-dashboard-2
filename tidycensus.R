library("tidyverse")
library("tidycensus")
library("ggplot2")
library("sf")
library("viridis")

census_api_key("d70672fd4eb7640a7bc59b0dbdfa3578a7fc0040")

utah <- get_acs(state = "UT", county = "Salt Lake", geography = "tract", 
                  variables = "B19013_001E", geometry = TRUE)

#View(utah)

utah %>%
  ggplot(aes(fill = estimate, color = estimate)) + 
  geom_sf() + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis(option = "magma") + 
  scale_color_viridis(option = "magma")




