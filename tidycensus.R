#library("tidyverse")
#library("tidycensus")
#library("ggplot2")
#library("sf")
#library("viridis")
library("leaflet")
library(tigris)
library(sp)
library(acs)
library(tmaptools)
opportunity_index <- data.frame(read.csv("OppurtunityIndex.csv"))
colnames(opportunity_index) <- c("tract", "index","GEOID")
#slc <- tracts(state = 'UT', county = c('Salt Lake'))
slc <- read_shape("shapefiles/tl_2015_49_tract.shp")


#plot(slc)
slc_merged <- geo_join(slc, opportunity_index, "GEOID", "GEOID",how = "inner")
pal <- colorNumeric(palette = "magma",domain = slc_merged$index)
popup <- paste0("Tract: ", as.character(slc_merged$tract))

opportunity_index_map = leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = slc_merged, 
              fillColor = ~pal(slc_merged$index), 
              fillOpacity = 0.7, 
              weight = 0.5, 
              smoothFactor = 0.2, 
              popup = popup) %>%
  addLegend(pal = pal, 
            values = slc_merged$index, 
            position = "topright", 
            title = "Opportunity Index in Salt Lake City")
opportunity_index_map






####code for rent
#utah_rent <- get_acs(state = "UT", county = "Salt Lake", geography = "tract", 
#                  variables = "B25111_001E", geometry = TRUE)

#pal <- colorNumeric(palette = "magma", 
#                    domain = utah_rent$estimate)

#rent_map <- utah_rent %>%
#  st_transform(crs = "+init=epsg:4326") %>%
#  leaflet(width = "100%") %>%
#  addProviderTiles(provider = "CartoDB.Positron") %>%
#  addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
#              stroke = FALSE,
#              smoothFactor = 0,
#              fillOpacity = 0.7,
#              color = ~ pal(estimate)) %>%
#  addLegend("bottomright", 
#            pal = pal, 
#            values = ~ estimate,
#            title = "Median Gross Rent",
#            labFormat = labelFormat(prefix = "$"),
#            opacity = 1)



###Code for Home Value
#utah_home <- get_acs(state = "UT", county = "Salt Lake", geography = "tract", 
#                     variables = "B25077_001", geometry = TRUE)

#pal <- colorNumeric(palette = "magma", 
#                    domain = utah_home$estimate)

#home_map <- utah_home %>%
#  st_transform(crs = "+init=epsg:4326") %>%
#  leaflet(width = "100%") %>%
#  addProviderTiles(provider = "CartoDB.Positron") %>%
#  addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
#              stroke = FALSE,
#              smoothFactor = 0,
#              fillOpacity = 0.7,
#              color = ~ pal(estimate)) %>%
#  addLegend("bottomright", 
#            pal = pal, 
#            values = ~ estimate,
#            title = "Median Home Value",
#            labFormat = labelFormat(prefix = "$"),
#            opacity = 1)

#home_map

