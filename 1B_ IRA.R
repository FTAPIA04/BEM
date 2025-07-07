##filtrado de base agrupada con ID ----
ETI_BQL_NAC <- agrupada %>% 
  filter(ID_SNVS_EVENTO_AGRP %in% c(7,8, 10, 443,441)) 


#armo base con SE_BEM----
ETI_BQL_NAC_BEM <-  ETI_BQL_NAC %>% 
    filter(ANIO == ANIO_max, SEMANA   %in% SE_BEM) 

#armo tabla con uni edad-----
grupo_etarioEBN <- ETI_BQL_NAC_BEM %>% 
  group_by(GRUPO,NOMBREEVENTOAGRP) %>% 
  summarise(total_Cantidad = sum(CANTIDAD, na.rm = TRUE))


#mutate eventos
grupo_etarioEBN <- grupo_etarioEBN %>% 
  mutate(
    NOMBREEVENTO=case_when(
      NOMBREEVENTOAGRP=="Bronquiolitis en menores de 2 años (sin especificar)"~"Bronquiolitis",
      NOMBREEVENTOAGRP=="Bronquiolitis en menores de 2 años ambulatorios"~"Bronquiolitis",
      NOMBREEVENTOAGRP=="Enfermedad tipo influenza (ETI)"~"ETI",
      NOMBREEVENTOAGRP=="Neumonía (sin especificar)"~"Neumonía",
      NOMBREEVENTOAGRP=="Neumonía en pacientes ambulatorios"~"Neumonía",
      TRUE ~ NOMBREEVENTOAGRP))


#mutate grupos de edad
grupo_etarioEBN <- grupo_etarioEBN %>% 
  mutate(
    GRUPO2=case_when(
      GRUPO== "< 6 m"~"Menor a 6 meses",
      GRUPO=="6 a 11 m"~"6 a 11 meses",
      GRUPO=="12 a 23 m"~"12 a 23 meses",
      GRUPO=="2 a 4"~"2 a 4 años",
      GRUPO=="5 a 9"~"5 a 9 años",
      GRUPO=="10 a 14"~"10 a 14 años",
      GRUPO=="15 a 19"~"15 a 19 años",
      GRUPO=="20 a 24"~"20 a 24 años",
      GRUPO=="25 a 34"~"25 a 34 años",
      GRUPO=="35 a 44"~"35 a 44 años",
      GRUPO=="45 a 64"~"45 a 64 años",
      GRUPO=="65 a 74"~"65 a 74 años",
      GRUPO==">= a 75"~"75 años y más",
      TRUE~GRUPO))



grupo_etarioEBN$GRUPO2 <- factor(grupo_etarioEBN$GRUPO2, 
                                       levels = c("Menor a 6 meses","6 a 11 meses",
                                                  "12 a 23 meses","2 a 4 años", "5 a 9 años",
                                                  "10 a 14 años", "15 a 19 años",
                                                  "20 a 24 años", "25 a 34 años", "35 a 44 años",
                                                  "45 a 64 años","65 a 74 años","75 años y más"))


#grafico----
IRA_grafico_grupoetario <- grupo_etarioEBN %>% 
  ggplot(aes(x=total_Cantidad, y=GRUPO2, fill = NOMBREEVENTO))+
  geom_bar(stat = "identity", width = 0.5)+
  labs(
    x = "Casos de IRA",
    y = "Grupos de edad",
    fill= "Evento") +
  scale_fill_manual(values = c(
    "Bronquiolitis" = "#ffce71",
    "Neumonía" = "#a7d554",  
    "ETI" = "#e0d258")) + 
  scale_x_continuous(
    breaks = seq(0, max(grupo_etarioEBN$total_Cantidad), by = 10),
    limits = c(0, max(grupo_etarioEBN$total_Cantidad) + 5)) +
  theme_classic () +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10), 
    axis.title.x = element_text(size = 10),  # Cambia el tamaño del título del eje X
    axis.title.y = element_text(size = 10),# Fuente para los textos del eje Y
    panel.border = element_blank(),  # Eliminar borde del panel
    axis.line = element_blank(),  # Eliminar líneas de los ejes
    axis.ticks = element_blank())  # Eliminar "guioncito" de los ejes
IRA_grafico_grupoetario














