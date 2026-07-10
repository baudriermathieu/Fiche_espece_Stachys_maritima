# Library à charger ----

library(dismo)
library(rgbif)
library(sf)
library(dplyr)
library(rnaturalearth)
library(ggplot2)
library(writexl)
library(readxl)
library(geodata)
library(ggspatial)

# Chargement des données d'occurence de Stachys maritima sur Gbif ----

donnees_occurence_stachys<-dismo::gbif(genus= 'Stachys', species= 'maritima'
                                        , geo= TRUE
                                        , download= TRUE
                                        , removeZeros = TRUE)

# Réduction du nombre de variables
data_stachys <- subset(donnees_occurence_stachys,select= c("lon", "lat", "elevation", "country", "day", 
                                                             "month", "year", "basisOfRecord", "locality"))

# Retrait des points sans coordonées GPS
data_stachys_clean <- data_stachys[!(is.na(data_stachys$lon)) & !(is.na(data_stachys$lat)),]

# Export des données dans un excel pour retirer les points aberrants (retrait ne fonctionne pas sur R)
write_xlsx(data_stachys_clean, "C:/Users/mathi/Documents/Cours/Master/S2/Floristique/Costa flora/donnes_stachys.xlsx")

# Chargement des données nettoyées ----
data_stachys_clean_2 <- read_excel("C:/Users/mathi/Documents/Cours/Master/S2/Floristique/Costa flora/donnes_stachys.xlsx")

# Conversion en objet sf pour les manipulations dans R
coord_stachys_sf <- st_as_sf(
  data_stachys_clean_2,
  coords = c("lon", "lat"),
  crs = 4326
)

# Production des cartographies ----

## Cartographie mondiale ----

# Import d'un fond de carte de tous les pays du monde
monde <- ne_countries(returnclass = "sf")

# Cartographie finale
ggplot() +
  geom_sf(data = monde, fill = "grey95", color = "grey70") +
  
  geom_sf(data = coord_stachys_sf, color = "red", size = 0.6, alpha = 0.5) +
  
  coord_sf(xlim = c(-10, 51), ylim = c(30, 50), expand = FALSE) +
  
  annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering) +
  
  annotation_scale(location = "bl", width_hint = 0.3) +
  
  theme_minimal() +
  
  theme(panel.background = element_rect(fill = "#b8dff5", colour = NA), panel.grid = element_blank(), plot.background = element_rect(fill = "white", colour = NA)) +
  
  labs(title = expression("Répartition mondiale de " * italic("Stachys maritima")), x = "Longitude", y = "Latitude")

## Cartographie régionale ----

### Téléchargement des limites administratives françaises, espagnoles et andorannes ----
france <- geodata::gadm("FRA", level = 2, path = "data")
espagne <- geodata::gadm("ESP", level = 2, path = "data")
andorre <- geodata::gadm("AND", level = 0, path = "data")

# Conversion en objet sf pour les manipulations dans R
france_sf <- st_as_sf(france)
espagne_sf <- st_as_sf(espagne)
andorre_sf <- st_as_sf(andorre)
andorre_sf <- andorre_sf[, c("geometry")]

# Fusion des limites des 3 pays
fr_es_an <- rbind(france_sf[, c("geometry")],espagne_sf[, c("geometry")],andorre_sf)

### Sélection des départements/provinces d'intérêt ----
zone_fr <- france_sf[france_sf$NAME_2 %in% c("Aude", "Pyrénées-Orientales"),]
zone_es <- espagne_sf[espagne_sf$NAME_2 %in% c("Girona", "Barcelona", "Tarragona"),]

# Fusion des zones d'intérêt françaises et espagnoles
zone_etude <- rbind(zone_fr, zone_es)

### Sélection des points présents dans notre zone d'étude ----

coord_stachys_local <- st_intersection(coord_stachys_sf, zone_etude)

### Représentation graphique ----

# Sélection des frontières internationales
pays_pour_frontiere <- geodata::gadm(c("FRA", "ESP"), level = 0, path = "data")
pays_pour_frontiere_sf <- st_as_sf(pays_pour_frontiere)
frontiere_fr_es <- st_cast(st_boundary(pays_pour_frontiere_sf), "LINESTRING")

# Cartographie finale
ggplot() +
  
  geom_sf(data = fr_es_an, fill = "grey95", color = "grey70", linewidth = 0.3) +
  
  # Zone d'étude en surbrillance
  geom_sf(data = zone_etude, fill = "#f6c85f", color = "black", linewidth = 0.3) +
  
  # Frontière en gras
  geom_sf(data = frontiere_fr_es, color = "black", linewidth = 0.6) +
  
  geom_sf(data = coord_stachys_local, color = "red", size = 1, alpha = 0.7) +
  
  coord_sf(xlim = c(0, 5), ylim = c(40.3, 43.8), expand = FALSE) +
  
  annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering) +
  
  annotation_scale(location = "br", width_hint = 0.3) +
  
  theme_minimal() +
  
  theme(panel.background = element_rect(fill = "#b8dff5", colour = NA), panel.grid = element_blank(), plot.background = element_rect(fill = "white", colour = NA)) +
  
  labs(title = expression("Répartition régionale de " * italic("Stachys maritima")), x = "Longitude", y = "Latitude")
