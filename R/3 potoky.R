library(sf)
library(dplyr)
library(sfnetworks)

# surový dibavod (v systému ing. Křováka)
ceska_voda <- st_read("./data/dibavod/A02_Vodni_tok_JU.shp") %>% 
  mutate(NAZ_TOK = stringi::stri_conv(NAZ_TOK, from = "cp-1250", to = "UTF-8"))
  
# body od Adama
excel <- readxl::read_excel("./data/WP2sites.xlsx") %>% 
  st_as_sf(coords = c("E", "N"), crs = 4326) %>% 
  st_transform(5514)

kves <- st_read("./data/KVES_Zlinsky.shp") %>% 
  mutate(kategorie = stringi::stri_conv(KATEGORIE, from = "cp-1250", to = "UTF-8")) %>% 
  select(kategorie)

# potoky zájmu jako index / tyto idčka vod nás zajímají (a jiné ne!)
aoi <- ceska_voda[st_nearest_feature(excel, ceska_voda), ] %>% 
  select(id =  TOK_ID) %>% 
  pull(id) %>% 
  unique()
 
# vody zájmu jako linestring
voda_zajmu <- ceska_voda  %>% 
  filter(TOK_ID %in% aoi) %>% 
  group_by(id = TOK_ID, name = NAZ_TOK) %>% 
  summarise(.groups = "drop") %>% 
  st_line_merge() # multi >> linestring
  
# rozsekání vod zájmu na úseky
useky_zajmu <- voda_zajmu %>% 
  as_sfnetwork() %>% 
  st_network_blend(excel) %>% 
  activate("edges") %>% 
  st_as_sf()

# buffery přeložené do KVESu
maly <- st_buffer(useky_zajmu, 100, endCapStyle = "FLAT", joinStyle = "MITRE") %>% 
  st_intersection(kves)

velky <- st_buffer(useky_zajmu, 150, endCapStyle = "FLAT", joinStyle = "MITRE") %>% 
  st_intersection(kves)

# odlít stranou pro vizuální kontrolu
st_write(excel, "./data/3potoky.gpkg", layer = "points", append = F)
st_write(useky_zajmu, "./data/3potoky.gpkg", layer = "sections", append = F)
st_write(maly, "./data/3potoky.gpkg", layer = "small_buffer", append = F)
st_write(velky, "./data/3potoky.gpkg", layer = "large_buffer", append = F)

# výstup
maly %>% 
  mutate(plocha = units::drop_units(st_area(.))) %>% 
  st_drop_geometry() %>%  # už jí nepotřebuju...
  group_by(from, to, name, kategorie) %>% 
  summarise(plocha = sum(plocha), .groups = "drop") %>% 
  tidyr::pivot_wider(values_from = plocha,
                     names_from = kategorie,
                     values_fill = 0) %>% 
  write.csv2("./output/3potoky.csv", row.names = F)
