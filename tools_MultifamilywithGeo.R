library(ggmap)

##### run this assitant tool when we have new Multifamily.xlsx

Multifamily <- read_excel("Data/Multifamily.xlsx", sheet = "Multi-Family Listings")
Multifamily$location <- paste0(Multifamily$Address, ", `Salt Lake City`", ", UT ", Multifamily$`Zip Code`)
geo <- geocode(location = Multifamily$location, output="latlon", source="google")
Multifamily$lon <- geo$lon
Multifamily$lat <- geo$lat
write.csv(Multifamily, "Data/new_multifamilywithgeo.csv")
