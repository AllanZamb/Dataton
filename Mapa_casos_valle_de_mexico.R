library(rgdal)
library(leaflet)
library(tidyverse)
library(stringi)
library(leaflet.extras)

#mexico <- readOGR(dsn = "Mapas/ZMVM1/ZMVM1.shp", 
mexico <- readOGR(dsn = "DatosCDMX/Mapas/ZMVM1/ZMVM1.shp", 
                  layer = "ZMVM1", use_iconv = TRUE, encoding = "UTF-8")

mexico_data <- mexico@data %>% 
  mutate(municipio = toupper(municipio), 
         municipio = stri_trans_general(municipio, "Latin-ASCII"))

casos_covid <- read.csv("DatosCDMX/BASES/casos_acum_mex_cdmx.txt") %>% 
  rename(municipio = MUNICIPIO)

  

mexico@data<-left_join(mexico_data, casos_covid, by = "municipio" )


leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%

 
  
  addPolygons(data = mexico, 
              fillOpacity = 0.5, 
              color = "black", 
              weight = 1,
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup = mexico$municipio,
              fillColor = ~colorQuantile("Blues", Casos)(Casos)
              ) %>% 
  
  addHeatmap( data = metro_mapas, lng = ~LONGITUD, lat = ~LATITUD,
              intensity = ~afluencia,  max = 1, blur = 20 ) 
              
  


