  library(tidyverse)
library(stringi)
library(leaflet)

estaciones <- read.csv("DatosCDMX/BASES/localizaciones_metro.csv", encoding = "UTF-8") %>%
  select(localizacion =geo_point_2d, Estacion = name) %>% 
  separate(localizacion, c("latitud", "longitud"), sep="," ) %>% 
  separate(Estacion, c("Estacion", "correspondencia1", "correspondencia2"), sep="_" ) %>% 
  select(-correspondencia1, -correspondencia2 ) %>% 
  mutate(Estacion = toupper(Estacion)) %>% 
  mutate(Estacion = stri_trans_general(Estacion , "Latin-ASCII")) %>% 
select(ESTACION= Estacion, LATITUD= latitud, LONGITUD=longitud) %>% 
  filter(!duplicated(ESTACION))
