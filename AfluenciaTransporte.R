#Limpieza base

library(tidyverse)
library(dint)
library(stringi)
library(ggpubr)
pacman::p_load(car, tidyverse, ggpubr, viridis, ggsci)

Afluencia_Total<- read.csv("DatosCDMX/BASES/Afluencia2020-2021.csv",
                           encoding = "UTF-8") %>% 
  mutate(fecha = as.Date(fecha, format = "%Y-%m-%d")) %>% 
  select(TRANSPORTE=organismo,FECHA=fecha,AFLUENCIA= afluencia_total_preliminar) %>% 
  mutate(FECHA= dint::as_date_yw(FECHA)) %>% 
  mutate(AFLUENCIA = gsub("-", NA, AFLUENCIA),
         AFLUENCIA = gsub(",", "", AFLUENCIA),
         AFLUENCIA = as.numeric(AFLUENCIA),
         TRANSPORTE= toupper(TRANSPORTE),
         TRANSPORTE= stri_trans_general(TRANSPORTE, "Latin-ASCII")) %>% 
  group_by(FECHA, TRANSPORTE) %>% 
  summarise(AFLUENCIA= sum(AFLUENCIA)) %>% 
  mutate(TRANSPORTE = recode(TRANSPORTE, 
                             "STC"= "METRO", 
                             "STE-TREN LIGERO"= "TREN LIGERO",
                            "STE-TROLEBUS"= "TROLEBUS", 
                            "STE-CABLEBUS" = "CABLEBUS")) 
 

 ggplot(data = Afluencia_Total) +
  geom_line(aes(x = FECHA, y =(AFLUENCIA), color = TRANSPORTE), size=1.0)+
  labs(title = "Movilidad de la CDMX en el transporte público",
       subtitle = "Del 01 de marzo 2020 a junio 2021",
       x= "Fecha de Registro por Semana",
       y="Afluencia", caption = "Fuente: Secretaría de movilidad (SEMOVI) / Afluencia preliminar ")+
   scale_y_continuous(labels = scales::comma)+
   theme_bw()+
   theme(plot.title = element_text(size = 15))+
   geom_vline(xintercept = c( as.Date("2020-03-23"), as.Date("2020-03-01"), as.Date("2020-09-16"),
                              as.Date("2020-12-31"),as.Date("2021-01-31")), linetype = 9, size =1, color="#298406") +
   scale_x_date_yw(name = "Fecha de Registro por número de semana",
                   labels = format_yw_short, breaks = date_yw(2020:2021, seq(1, 53, by = 1))) +
   
   theme(text=element_text(size=10,hjust = 0.5),
         
         axis.text.x = element_text(angle=90,
                                    hjust = 1, 
                                    size = 10),
         
         legend.text=element_text(size=12),
         
         axis.text.y = element_text(hjust = 1, 
                                    size = 12),
         
         plot.title = element_text(hjust = 0.5), 
         legend.position = "bottom")

