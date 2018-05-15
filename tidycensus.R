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

save(opportunity_index_map, file= "./Data/rds/opportunity_index_map.rds")




