library(sf)
library(scatterpie)
library(ggmap)
library(tidyverse)


#DATA INPUT 

coordinates<-readxl::read_xlsx("./data/GF_regional_scale_locs.xlsx")%>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% # takhle je zadáno
  st_transform(3857) %>% # takhle to potřebujeme
  mutate(xcoord = st_coordinates(.)[, "X"] %>% unname(),
         ycoord = st_coordinates(.)[, "Y"] %>% unname()) %>%  
  st_drop_geometry() 

ceska_voda <- st_read("./data/dibavod/A02_Vodni_tok_JU.shp") %>% 
  st_transform(4326) %>% # souřadnicový systém WGS84 = jako GPS
  # oříznout na Vsetínsko / jako stažená basemap
  st_crop(xmin = 17.5,
          xmax = 18.5,
          ymin = 49.1,
          ymax = 49.6) %>% 
  st_transform(3857) # souřadnicový systém pro ggmap (legitimní)

# https://www.geoportal.sk/sk/inspire/ukladacie-sluzby/ Hydrografia jako geopackage
slovenska_voda <- st_read("./data/Hydrografia/INSPIRE_HY.gpkg",
                          layer = "Watercourse") %>% 
  st_transform(4326) %>% # souřadnicový systém WGS84 = jako GPS
  # oříznout na Vsetínsko / jako stažená basemap
  st_crop(xmin = 17.5,
          xmax = 18.5,
          ymin = 49.1,
          ymax = 49.6) %>% 
  st_transform(3857) # souřadnicový systém pro ggmap (legitimní)

# download basemap
basemap <- get_stamenmap(bbox = c(left = 17.5,
                                  bottom = 49.1,
                                  right = 18.5,
                                  top = 49.6),
                         zoom = 11, # treba trochu ladit....
                         maptype = "terrain")

# tohle je zajímavé! ggmap je totiž "trošku zprasená" (z dobrého důvodu, ale nehraje fér s ostatními)
# https://stackoverflow.com/questions/47749078/how-to-put-a-geom-sf-produced-map-on-top-of-a-ggmap-produced-raster
# Define a function to fix the bbox to be in EPSG:3857
ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))
  
  # Coonvert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
  
  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}

# upravit basemap ze zmršené 4326 do legitímní 3857
basemap <- ggmap_bbox(basemap)

# merge points and basemaps into output
ggmap(basemap) +
  geom_sf(data = ceska_voda, 
          color = "steelblue", 
          alpha = 1/3, 
          inherit.aes = F) +
  geom_sf(data = slovenska_voda, 
          color = "steelblue", 
          alpha = 1/3, 
          inherit.aes = F) +
  geom_scatterpie(data = coordinates, 
                  aes(x = xcoord, 
                      y = ycoord,
                      group = locality),
                  color = NA,
                  cols = c("CWEA", "EEQ", "CEA", "CWED")) + 
  theme_void()

ggsave("map-alt.pdf")
