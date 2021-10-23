
library(stringi)
library(dint)
library(tidyverse)

metro<-read.csv("DatosCDMX/BASES/afluencia metrof.csv", encoding = "UTF-8") %>% 
mutate(
        sem_epi = lubridate::epiweek(fecha), 
         nio_epi = lubridate::epiyear(fecha)) %>% 
  mutate(fecha = as.Date(fecha, format = "%d/%m/%Y")) %>% 
           select(FECHA=fecha, LINEA= linea, ESTACION=estacion,AFLUENCIA=afluencia) %>% 
  mutate( ESTACION= toupper(ESTACION),
          ESTACION= stri_trans_general(ESTACION, "Latin-ASCII"), LINEA= toupper(LINEA))



##Hacemos merge


metroD<- read.csv("DatosCDMX/BASES/delegaciones_metro.csv", encoding = "UTF-8") %>% 
  select(ESTACION=Estacion, ALCALDIA=Alcaldia) %>% 
  filter(!duplicated(ESTACION))


##FUNCION WHICH
unique(metroD$ESTACION)

metroD$ESTACION[which(metroD$ESTACION%in% c("ZOCALO/TENOCHTITLAN" ))]<- "ZOCALO"
metroD$ESTACION[which(metroD$ESTACION%in% c("BOULEVARD PUERTO AEREO" ))]<- "BLVD. PUERTO AEREO"
metroD$ESTACION[which(metroD$ESTACION%in% c("HOSPITAL 20 DE NOVIEMBRE"  ))]<- "20 DE NOVIEMBRE"
metroD$ESTACION[which(metroD$ESTACION%in% c("PERIFERICO ORIENTE"))]<- "PERIFERICO ORIENTE"
metroD$ESTACION[which(metroD$ESTACION%in% c("DEPORTIVO 18 DE MARZO" ))]<- "DEPTVO. 18 DE MARZO"
metroD$ESTACION[which(metroD$ESTACION%in% c("ETIOPIA-PLAZA DE LA TRANSPARENCIA" ))]<- "ETIOPIA"
metroD$ESTACION[which(metroD$ESTACION%in% c("MIGUEL ANGEL DE QUEVEDO"  ))]<- "MIGUEL A. DE Q." 
metroD$ESTACION[which(metroD$ESTACION%in% c("VIVEROS-DERECHOS HUMANOS"  ))]<- "VIVEROS"
metroD$ESTACION[which(metroD$ESTACION%in% c("INSTITUTO DEL PETROLEO"   ))]<- "INST. DEL PETROLEO"
metroD$ESTACION[which(metroD$ESTACION%in% c("UAM-AZCAPOTZALCO" ))]<- "AZCAPOTZALCO" 
metroD$ESTACION[which(metroD$ESTACION%in% c("FERRERIA/ARENA CIUDAD DE MEXICO" ))]<-  "FERRERIA" 
metroD$ESTACION[which(metroD$ESTACION%in% c("LA VILLA-BASILICA"))]<-  "LA VILLA-BASILICA"  
metroD$ESTACION[which(metroD$ESTACION%in% c( "SAN PEDRO DE LOS PINOS"))]<-  "SAN PEDRO LOS PINOS" 
metroD$ESTACION[which(metroD$ESTACION%in% c( "GARIBALDI-LAGUNILLA" ))]<-  "GARIBALDI"  
metroD$ESTACION[which(metroD$ESTACION%in% c( "SAN JUAN DE LETRAN"  ))]<-  "SAN JUAN LETRAN" 
metroD$ESTACION[which(metroD$ESTACION%in% c( "UAM-I"  ))]<-  "U A M  I" 
metroD$ESTACION[which(metroD$ESTACION%in% c( "MIXIUHCA"  ))]<-  "MIXIUHCA"
metroD$ESTACION[which(metroD$ESTACION%in% c( "ECATEPEC"  ))]<-  "ECATEPEC"  
metroD$ESTACION[which(metroD$ESTACION%in% c( "RICARDO FLORES MAGON"  ))]<-  "RICARDO FLORES MAGON" 
 


MetroDelegacion<-left_join(metro, metroD) %>% 
 mutate(FECHA= dint::as_date_yw(FECHA)) %>% 
  group_by(FECHA, LINEA, ESTACION, ALCALDIA) %>% 
  summarise(AFLUENCIA= sum(AFLUENCIA, na.rm = T))
Metrofinal<- left_join(MetroDelegacion, estaciones)
unique(metro$ESTACION)
