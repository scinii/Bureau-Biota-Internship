library(openxlsx) 
library(sf)
library(ggOceanMaps)

setwd('C:\\Users\\rober\\Desktop\\Internship') # set working directory

lakes_location = read.xlsx(xlsxFile = "data_lakes.xlsx", sheet = "locations")
#lakes_location_sf = st_as_sf(lakes_location, coords=c("lon","lat"))
lakes_trans = transform_coord(lakes_location)
lakes_trans$ID = lakes_location$ID

basemap(limits = c(10,13,78.8,79.2), crs = 3995) + 
 geom_point(data = lakes_trans, aes(x = lon, y = lat, color = ID))