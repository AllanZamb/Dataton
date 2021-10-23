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
  
  scale_y_continuous(labels = scales::comma) +
  scale_x_date_yq(name = "Semana Epi",
                  labels = format_ym_short,
                  breaks = date_ym(2012:2021, seq(1, 12, by = 2)))+
  
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


Movilidad_global_Google %>% 
  filter(ENTIDAD == "CIUDAD DE MEXICO") %>% 
  gather(., `Tipo de movilidad`,`Porcentaje de Movilidad`, 3:8) %>% 
  mutate(Fecha = dint::as_date_yw(Fecha)) %>% 
  group_by(Fecha, `Tipo de movilidad` ) %>% 
  summarise(`Porcentaje de Movilidad` = mean(`Porcentaje de Movilidad`)) %>% 
  
 
  ggplot()+
  geom_bar(aes(x=Fecha, y = (`Porcentaje de Movilidad`), fill = `Tipo de movilidad`), stat = "identity")+
  
  #geom_line(aes(x=Fecha, y = (`Porcentaje de Movilidad`/100), color = `Tipo de movilidad`), size = 1.5)+
  
  scale_x_date_yw(name = "Fecha de Registro por número de semana",
                  labels = format_yw_short, breaks = date_yw(2020:2021, seq(1, 53, by = 2)))+

  #scale_y_continuous(labels = scales::percent)+
  theme_bw()+
  theme(text=element_text(size=10,hjust = 0.5),
        
        axis.text.x = element_text(angle=90,
                                   hjust = 1, 
                                   size = 10),
        
        legend.text=element_text(size=12),
        
        axis.text.y = element_text(hjust = 1, 
                                   size = 12),
        
        plot.title = element_text(hjust = 0.5)) + 
  
  labs(y = "Porcentaje de cambio en la movilidad")+
  
  geom_vline(xintercept = c( as.Date("2020-03-23"), 
                             as.Date("2020-06-01"), 
                             
                             as.Date("2020-12-20"), 
                             as.Date("2021-02-14"),
                             as.Date("2021-02-14")), 
             
             linetype = 8, size =.5) +
  
  geom_hline(yintercept = 0 , linetype = 1, size =.5)
