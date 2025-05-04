library(openxlsx) 
library(sf)
library(ggOceanMaps)
library(ggspatial)
library(ggrepel)

setwd('C:\\Users\\rober\\Documents\\GitHub\\Bureau-Biota-Internship') # set working directory

lakes_location = read.xlsx(xlsxFile = "data_lakes.xlsx", sheet = "locations")


basemap(limits = c(11.5, 13, 78.8, 79),shapefiles = "Svalbard") + 
  theme(panel.background = element_rect(fill = "lightblue"),panel.ontop = FALSE) +
  geom_spatial_point(data = lakes_location, aes(x = lon, y = lat), color='red') + 
  geom_spatial_text_repel(data = lakes_location, aes(x = lon, y = lat, label = Location), max.overlaps = Inf) 
