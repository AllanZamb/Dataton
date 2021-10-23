library(tidyverse)
library(stringi)
library(dint)
Ingresos <-read.csv("https://datos.cdmx.gob.mx/dataset/70166aa5-4ba9-43a5-9fe1-9ced9245fbd9/resource/b23a7eb4-3a3e-4a19-bd06-83d18a002ff2/download/ingresos_del_sistema_de_transporte_colectivo_metro.csv", encoding = "UTF-8") %>%
#Ingresos <-read.csv("DatosCDMX/BASES/Ingresos Lineas.csv", encoding = "UTF-8") %>%
  mutate(FECHA = as.Date(FECHA, format = "%d/%m/%Y")) %>% 
  gather(., LINEA , AFLUENCIA, 3:length(.)) %>% 
  mutate(LINEA =  gsub( "_", " ", LINEA ))


Ingresos %>% 
  filter(FECHA >= "2019-01-01") %>% 
  mutate(FECHA = dint::as_date_yw(FECHA)) %>% 
  group_by(FECHA, TIPO_INGRESO, LINEA) %>% 
  summarise(AFLUENCIA = sum(AFLUENCIA, na.rm = T)) %>% 
  ggplot()+
  #geom_bar(aes(x=FECHA, y = AFLUENCIA, fill = TIPO_INGRESO), stat = "identity")+
 
  geom_line(aes(x=FECHA, y = (AFLUENCIA), color = TIPO_INGRESO))+ 
  scale_x_date_yw(labels = format_yw_iso)+
  scale_y_continuous(labels = scales::comma)+
  geom_vline(xintercept = c( as.Date("2020-11-09") ), linetype = 8, size =.5)+
  facet_wrap(~LINEA, scales = "free")+
  theme_bw()
  


Ingresos %>% 
  filter(FECHA >= "2019-01-01", TIPO_INGRESO != "Tarjetas" ) %>% 
  mutate(FECHA = dint::as_date_ym(FECHA)) %>% 
  group_by(FECHA, TIPO_INGRESO) %>% 
  summarise(AFLUENCIA = sum(AFLUENCIA, na.rm = T)) %>% 
  mutate(Ingresos = AFLUENCIA *5) %>% 

  ggplot()+
  geom_line(aes(x=FECHA, y = AFLUENCIA, color = TIPO_INGRESO))+

  scale_y_continuous(labels = scales::comma)+
  
  
  
  # scale_x_date_yw(name = "Semana Epi",
  #                 labels = format_yw_short, 
  #                 breaks = date_yw(2019:2021, seq(1, 53, by = 2)))+
  
  scale_x_date_yw(name = "Fecha de Registro",
                  
                  labels = format_ym_short, 
                  breaks = date_ym(2019:2021, seq(1, 12, by = 1)))+
  
  theme_bw()+
  theme(text=element_text(size=10,hjust = 0.5),
        
        axis.text.x = element_text(angle=90,
                                   hjust = 1, 
                                   size = 10),
        
        legend.text=element_text(size=12),
        
        axis.text.y = element_text(hjust = 1, 
                                   size = 12),
        
        plot.title = element_text(hjust = 0.5)) + 
  
  geom_vline(xintercept = c( as.Date("2020-05-01")  ), linetype = 8, size =.5)+
  geom_vline(xintercept = c( as.Date("2020-11-09") ), linetype = 8, size =.5) + 
  
  labs()







Ingresos %>% 
  filter(
    #FECHA >= "2019-01-01",
    TIPO_INGRESO != "Tarjetas" ) %>% 
  mutate(FECHA = dint::as_date_ym(FECHA)) %>% 
  group_by(FECHA, TIPO_INGRESO) %>% 
  summarise(AFLUENCIA = sum(AFLUENCIA, na.rm = T)) %>% 
  mutate(Ingresos = AFLUENCIA *5) %>% 
  
  ggplot()+
  geom_line(aes(x=FECHA, y = AFLUENCIA, color = TIPO_INGRESO))+
  
  scale_y_continuous(labels = scales::comma)+
  
  
  
  # scale_x_date_yw(name = "Semana Epi",
  #                 labels = format_yw_short, 
  #                 breaks = date_yw(2019:2021, seq(1, 53, by = 2)))+
  
  scale_x_date_yw(name = "Semana Epi",
                  labels = format_ym_short,
                  breaks = date_ym(2012:2021, seq(1, 12, by = 1)))+
  
  theme_bw()+
  theme(text=element_text(size=10,hjust = 0.5),
        
        axis.text.x = element_text(angle=90,
                                   hjust = 1, 
                                   size = 10),
        
        legend.text=element_text(size=12),
        
        axis.text.y = element_text(hjust = 1, 
                                   size = 12),
        
        plot.title = element_text(hjust = 0.5)) + 
  
  geom_vline(xintercept = c( as.Date("2020-05-01")  ), linetype = 8, size =.5)+
  geom_vline(xintercept = c( as.Date("2020-11-09") ), linetype = 8, size =.5)







Linea_Ingresos <-Ingresos %>% 
  filter(FECHA >= "2019-01-01") %>% 
  mutate(FECHA = dint::as_date_ym(FECHA)) %>% 
  group_by(FECHA) %>% 
  summarise(AFLUENCIA = sum(AFLUENCIA, na.rm = T)) %>% 
  mutate(Ingresos = AFLUENCIA *5) 


Linea_Ingresos %>% 
  arrange()


Linea_Ingresos %>%  
  ggplot()+
  geom_line(aes(x=FECHA, y = AFLUENCIA, color= "Metro"))+
  
  scale_color_manual(values = c("Metro" = "Orange"))+
  
  scale_y_continuous(labels = scales::comma)+
  
  scale_x_date_yw(name = "Semana Epi",
                  labels = format_yw_short, breaks = date_yw(2018:2021, seq(1, 53, by = 2)))+
  
  theme_bw()+
  theme(text=element_text(size=10,hjust = 0.5),
        
        axis.text.x = element_text(angle=90,
                                   hjust = 1, 
                                   size = 10),
        
        legend.text=element_text(size=12),
        
        axis.text.y = element_text(hjust = 1, 
                                   size = 12),
        
        plot.title = element_text(hjust = 0.5)) + 
  
  geom_vline(xintercept = c( as.Date("2020-03-23"), 
                             as.Date("2020-06-01"), 
                             
                             as.Date("2020-12-20"), 
                             as.Date("2021-02-14")), 
             
             linetype = 8, size =.5) +
  
  annotate("segment",
           x= as_date_yw("2020-06-01")+5,
           xend = as_date_yw("2020-06-01"),
           y= max(Linea_Ingresos$AFLUENCIA)-15,
           yend = max(Linea_Ingresos$AFLUENCIA) ,
           
           colour = "black",
           size =.5,
           alpha=1,
           arrow = arrow(length = unit(0.015, "npc"), type = "closed")) + 
  
  geom_text(aes(x=as_date_yw("2020-06-01")+5,
                y = max(Linea_Ingresos$AFLUENCIA) -20,
                label = "Jornada de sana distancia"), size =2 , color = "black") 
  
  # geom_rect(data=semaforo_entidad,aes(xmin = Semana_Ini, xmax = Semana_Fin, fill = Semaforo),
  #           ymin = -Inf, ymax = Inf, alpha = 0.2)+
  
  


