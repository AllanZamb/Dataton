library(leaflet)
library(KernSmooth)
library(leaflet.extras)

metro_mapas<- Metrofinal %>% 
  #filter(ALCALDIA %in% c("CUAUHTEMOC")) %>% 
  mutate(LONGITUD = as.numeric(LONGITUD),
         LATITUD = as.numeric(LATITUD)) %>% 
  group_by(ESTACION, LATITUD, LONGITUD) %>% 
  summarise(AFLUENCIA = sum(AFLUENCIA))
  

leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(data = metro_mapas, ~LONGITUD, ~LATITUD, 
                   color = "red", radius = (metro_mapas$AFLUENCIA/200000) ) 



#ESTIMADOR DE DENSIDAD PARA LA VARIABLE DE LOS CASOS

#El estimador promedia la relacion de distancia en vecinos cercanos (clusters)
#Cada estimador crea una linea con respecto a sus vecinos

metro_benito_juarez <-metro_mapas %>% 
  ungroup() %>% 
  select(LONGITUD, LATITUD) %>% 
  filter(!duplicated(.))

metro_benito_juarez <- as.data.frame(metro_benito_juarez) %>% 
  drop_na()

estimador_densidad_casos <- bkde2D(metro_benito_juarez, 
                                   bandwidth = c(bw.ucv(metro_benito_juarez$LONGITUD), 
                                                 bw.ucv(metro_benito_juarez$LATITUD)))

coordenadas_x <- estimador_densidad_casos$x1
coordenadas_y <- estimador_densidad_casos$x2
matriz_densidad <- estimador_densidad_casos$fhat

contorno_casos <- contourLines(coordenadas_x, coordenadas_y, matriz_densidad)

################################################################################

#Creamos el mismo leaflet pero con una línea de densidad añadida al perímetro de las bombas de agua
metro_mapas <- metro_mapas %>% 
  drop_na() %>% 
  mutate(afluencia = (AFLUENCIA/9000000))

afluencia<-rescale(metro_mapas$AFLUENCIA, to =c(0,2))

leaflet() %>% 
  addTiles() %>% 
  addHeatmap( data = metro_mapas, lng = ~LONGITUD, lat = ~LATITUD,
             intensity = ~afluencia,  max = 1, blur = 20 ) 
  
addPolylines(contorno_casos[[1]]$x, contorno_casos[[1]]$y,color = "red") %>%
  addPolygons(contorno_casos[[7]]$x, contorno_casos[[7]]$y, fillColor = "yellow", stroke = FALSE) %>% 
addPolylines(contorno_casos[[2]]$x, contorno_casos[[2]]$y,color = "red") %>%
addPolylines(contorno_casos[[3]]$x, contorno_casos[[3]]$y,color = "red") %>%
addPolylines(contorno_casos[[4]]$x, contorno_casos[[4]]$y,color = "red") %>%
addPolylines(contorno_casos[[5]]$x, contorno_casos[[5]]$y,color = "red") %>% 
  addPolylines(contorno_casos[[6]]$x, contorno_casos[[6]]$y,color = "red") %>% 
  addPolylines(contorno_casos[[7]]$x, contorno_casos[[7]]$y,color = "red") 
  

  
