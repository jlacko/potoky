library(sf)
library(dplyr)
library(tidyr)
library(leaflet)

# načíst data
kves <- st_read("./data/KVES_Zlinsky.shp") 
data <- st_read("./data/export.gpx")

# začistit češtinu / mor na ty vaše rody!
kves <- kves %>% 
  mutate(kategorie = iconv(KATEGORIE, from = "CP1250", to = "UTF-8")) %>% 
  select(kategorie)

data <- data %>% 
  select(name) %>% 
  st_transform(st_crs(kves))

# okolí x metrů kolem pozorování
okoli <- st_buffer(data, units::set_units(200, "m")) %>% 
  mutate(plocha = st_area(.))

# průsečík bufferů a kves
prusecik <- okoli %>% 
  st_intersection(kves) %>% 
  # podíl ekosystému dané kategorie na celkové ploše
  mutate(podil = units::drop_units(st_area(.) / plocha)) %>% 
  st_drop_geometry() %>% 
  select(name, kategorie, podil) %>% 
  # vysčítat kategorie po bufferu - jeden buffer + jedna kategorie = jedno číslo
  group_by(name, kategorie) %>% 
  summarise(podil = sum(podil))

vystup <- prusecik %>% 
  mutate(kategorie = iconv(kategorie, to = "CP1250", from = "UTF-8")) %>% # blééééjt!
  pivot_wider(names_from = kategorie,
              values_from = podil,
              values_fill = 0)

readr::write_csv2(vystup, "./output/kategorie.csv")

# mapový výstup
okoli %>% 
  st_intersection(kves) %>% 
  st_transform(4326) %>% 
  mapview::mapview(zcol = "kategorie") %>% 
  mapview::mapshot2("./output/kategorie.html")
  