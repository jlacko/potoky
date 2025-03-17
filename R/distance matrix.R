#
# načte data z Dibavod, připraví distanční matici; tu uloží a předvede namátkovou kontrolu
#

library(dplyr)
library(sf)
library(sfnetworks)

lokality <- readxl::read_excel("./data/lokality Vsetinsko.xlsx") %>% 
  st_as_sf(coords = c("lat", "long"), crs = 4326) %>% 
  st_transform(5514) # systém inž. Křováka / kvůli metrům, a konzistenci...

# zdroj dat:
# https://www.dibavod.cz/index.php?id=27
voda <- st_read("./data/dibavod/A02_Vodni_tok_JU.shp") %>% 
  st_cast("LINESTRING")

# připravit síť jako graf
vodni_sit <- as_sfnetwork(voda, directed = F) %>% 
  activate("nodes") %>% 
  st_network_blend(lokality) %>% # lokality jako body v síti
  activate("edges") %>% 
  mutate(weight = edge_length()) 

# spočítat matici
dist_matrix <- st_network_cost(vodni_sit,
                               from = st_geometry(lokality),
                               to = st_geometry(lokality))

# lokality jako názvy řádek a sloupců
rownames(dist_matrix) <- lokality$kod
colnames(dist_matrix) <- lokality$kod

# začištění nekonečna 
dist_matrix[dist_matrix == Inf] <- NA

# o všem podat zprávu...
write.csv2(as.data.frame(dist_matrix),
           "matice.csv",
           row.names = T)

# ještě namátková kontrola
start <- "BYS1"
cil <- "TRN1"

# cesta mezi dvěma lokalitami (určenými názvem) jako indexy v síti
cesta_idx <- st_network_paths(vodni_sit,
                          from = lokality[lokality$kod == start, ],
                          to = lokality[lokality$kod == cil, ])

# vlastní vizuální kontrola
cesta<- vodni_sit %>% 
  activate("edges") %>% 
  slice(unlist(cesta_idx$edge_paths)) %>% 
  st_as_sf() 

#vizuální kontrola "krátkosti"
mapview::mapview(cesta)

# numerická kontrola - hodnota z matice vs délka cesty jako čáry na mapě
dist_matrix[start, cil]
st_length(st_combine(cesta))


