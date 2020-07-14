################################################################################
# Play with shapefiles
#
# Author: Stefan Schliebs
# Created: 2020-06-22 09:30:52
################################################################################

library(rgdal)
library(leaflet)
library(sf)


system.time(
  sh_sa <- st_read(
    "data/shapes/statistical-area-2-2018-generalised.shp", 
    layer = "statistical-area-2-2018-generalised"
  )
)

system.time(
  sh_sa <- rgdal::readOGR(
    "data/shapes/statistical-area-2-2018-generalised.shp", 
    layer = "statistical-area-2-2018-generalised"
  )
)

sh_sa_simple <- rmapshaper::ms_simplify(sh_sa, keep = 0.05)

object.size(sh_sa)
object.size(sh_sa_simple)

names(sh_sa_simple)

leaflet(sh_sa_simple[sh_sa_simple$LAND_AREA_ > 0,]) %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  # addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(
    color = "#FFFFFF", weight = 1, smoothFactor = 0.5,
    opacity = 0.5, fillOpacity = 0.05,
    highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)
  )


