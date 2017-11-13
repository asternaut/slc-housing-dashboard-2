library("tidyverse")
library("tidycensus")
library("ggplot2")
library("sf")
library("viridis")
library("leaflet")
library("stringr")

census_api_key("d70672fd4eb7640a7bc59b0dbdfa3578a7fc0040")
####code for rent
utah_rent <- get_acs(state = "UT", county = "Salt Lake", geography = "tract", 
                  variables = "B25111_001E", geometry = TRUE)

pal <- colorNumeric(palette = "magma", 
                    domain = utah_rent$estimate)

utah_rent %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal(estimate)) %>%
  addLegend("bottomright", 
            pal = pal, 
            values = ~ estimate,
            title = "Median Gross Rent",
            labFormat = labelFormat(prefix = "$"),
            opacity = 1)


utah_rent <- get_acs(state = "UT", county = "Salt Lake", geography = "tract", 
                     variables = "B25111_001E", geometry = TRUE)

pal <- colorNumeric(palette = "magma", 
                    domain = utah_rent$estimate)
###Code for Home Value
utah_home <- get_acs(state = "UT", county = "Salt Lake", geography = "tract", 
                     variables = "B25077_001", geometry = TRUE)

pal <- colorNumeric(palette = "magma", 
                    domain = utah_home$estimate)

utah_home %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal(estimate)) %>%
  addLegend("bottomright", 
            pal = pal, 
            values = ~ estimate,
            title = "Median Home Value",
            labFormat = labelFormat(prefix = "$"),
            opacity = 1)
#v15 <- load_variables(2015, "acs5", cache = TRUE)
#View(v15)
