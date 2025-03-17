#
# dohledá k lokalitě nadmořskou výšku / definovanou jako nejbližší vrstevnici z mapy 1:50K
# 

library(dplyr)
library(sf)

lokality <- readxl::read_excel("./data/lokality Vsetinsko.xlsx") %>% 
  st_as_sf(coords = c("lat", "long"), crs = 4326) %>% 
  st_transform(5514) # systém inž. Křováka / kvůli metrům, a konzistenci...

# zdroj dat:
# https://geoportal.cuzk.cz/ZAKAZKY/Data50/terenniRelief.zip
vrstevnice <- st_read("./data/terenniRelief/Vrstevnice.shp")

# výška metodou "na prasáka" - extrapolace by byla přesnější...
lokality$vyska <- vrstevnice$ELEVATION[st_nearest_feature(lokality, vrstevnice)]