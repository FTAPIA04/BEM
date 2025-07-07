#Filtrado agrupada ID NEUMONIA-----

NEUMONIA <- agrupada %>% 
  filter(ID_SNVS_EVENTO_AGRP %in% c(10, 441))

#objeto cantidad de NEUMONIAS notificadas en las SE del BEM-----
NEUMONIA_cantidad_SE_BEM <- NEUMONIA %>%
  filter(ANIO == ANIO_max, SEMANA   %in% c(SE_BEM)) %>%
  summarise(total_Cantidad = sum(CANTIDAD, na.rm = TRUE))


#selecciono las SE del año pasado (es para hacer la variacion porcentual del set de indicadores)-----
NEUMONIA_cantidad_SE_BEM_anioanterior <- NEUMONIA %>%
  filter(ANIO == anio_anterior, SEMANA %in% c(SE_BEM)) %>% 
  summarise(total_Cantidad = sum(CANTIDAD, na.rm = TRUE)) 


#variacion porcentual 
NEUMONIA_variacion_porcentual <- 
  round((NEUMONIA_cantidad_SE_BEM - NEUMONIA_cantidad_SE_BEM_anioanterior)/NEUMONIA_cantidad_SE_BEM_anioanterior*100,1)

#creo objeto de aumento o disminución
NEUMONIA_texto_variacion <- ifelse(NEUMONIA_variacion_porcentual < 0, "una disminución", "un aumento")




#suma total NEUMONIA----
NEUMONIA_total_evolutivo <- NEUMONIA %>% 
  summarise(Total = sum(CANTIDAD))



#grafico acumulado grupos etarios-------
#distribución SE por grupos etarios

#Armar una tabla para hacer un graf de NEUMONIA por grupos etarios SE del BEM
NEUMONIA_tabla_grupoetario_acumulado <- NEUMONIA %>% 
  group_by(ANIO, SEMANA, GRUPO) %>%  
  summarize(Total = sum(CANTIDAD, na.rm = TRUE), .groups = "drop") %>%  # Sumar por grupo etario
  mutate(ANIO_SE = paste(SEMANA, ANIO, sep = "-")) %>% 
  arrange(ANIO, SEMANA) %>% 
  mutate(ANIO_SE = factor(ANIO_SE, levels = unique(ANIO_SE))) %>% 
  tidyr::complete(SEMANA = 1:max(SEMANA, na.rm = TRUE), fill = list(Total = 0)) %>%
  as.data.frame()

# Ver la tabla y chequeo para ver que esté ok

#agrupo segun grupos etarios del BEM 
NEUMONIA_tabla_grupoetario_acumulado <- NEUMONIA_tabla_grupoetario_acumulado %>%
  mutate(
    GRUPO_2 = case_when(
      GRUPO == "< 6 m"~ "< 15 años",
      GRUPO == "6 a 11 m"~ "< 15 años",
      GRUPO == "12 a 23 m"~ "< 15 años",
      GRUPO == "2 a 4"~ "< 15 años",
      GRUPO == "5 a 9"~ "< 15 años",
      GRUPO == "10 a 14"~ "< 15 años",
      GRUPO == "15 a 19"~ "15 a 24 años",
      GRUPO == "20 a 24"~ "15 a 24 años",
      GRUPO == "25 a 34"~ "25 a 44 años",
      GRUPO == "35 a 44"~ "25 a 44 años",
      GRUPO == "45 a 54"~ "45 a 64 años",
      GRUPO == "45 a 64"~ "45 a 64 años",
      GRUPO == "55 a 64"~ "45 a 64 años",
      GRUPO == "65 a 74"~ "65 años y más",
      GRUPO == ">= a 75"~ "65 años y más",
      GRUPO == "Edad Sin Esp."~ "Sin especificar",
      TRUE ~ GRUPO))


NEUMONIA_tabla_grupoetario_acumulado$GRUPO_2 <- factor(NEUMONIA_tabla_grupoetario_acumulado$GRUPO_2, 
                                                  levels = c("< 15 años","15 a 24 años",
                                                             "25 a 44 años","45 a 64 años", 
                                                             "65 años y más","Sin especificar"))



NEUMONIA_tabla_grupoetario_acumulado <- NEUMONIA_tabla_grupoetario_acumulado %>% 
  mutate(GRUPO_2 = factor(GRUPO_2, levels = rev(c(
    "< 15 años",
    "15 a 24 años",  
    "25 a 44 años",  
    "45 a 64 años", 
    "65 años y más",
    "Sin especificar"))))



# Crear gráfico de columnas apiladas
NEUMONIA_grafico_grupoetario_acumulado <- NEUMONIA_tabla_grupoetario_acumulado %>% 
  #  filter(!is.na(GRUPO_2)) %>%  # Eliminar filas con NA en GRUPO_2
  ggplot(aes(x = ANIO_SE, y = Total, fill = GRUPO_2)) +
  geom_bar(stat = "identity", position = "fill") + #position fill es para apilado al 100%
  scale_x_discrete(
    breaks = levels(NEUMONIA_tabla_grupoetario_acumulado$ANIO_SE)[seq(1, length(levels(NEUMONIA_tabla_grupoetario_acumulado$ANIO_SE)), by = 5)],
    expand = c(0, 0)) +
  scale_y_continuous(
    labels = scales::percent,  # Cambiar el formato de las NEUMONIAquetas a porcentaje
    breaks = seq(0, 1, by = 0.1))+ # Poner los breaks del eje Y entre 0 y 1, con intervalos de 0.1
  scale_fill_manual(values = c(
    "< 15 años" = "#2f4858",
    "15 a 24 años" = "#4e5a79",  
    "25 a 44 años" = "#7f678f",  
    "45 a 64 años" = "#b47396", 
    "65 años y más" = "#e0848c",
    "Sin especificar"= "#f9a17b")) +  
  labs(
    x = "SE-año",
    y = "% de casos de NEUMONIA",
    fill = "Grupo de edad") +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 25, angle = 90, hjust = 1),  # Rotar etiquetas en X
    axis.text.y = element_text(size = 25),  # Fuente para los textos del eje Y
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    strip.text = element_text(size = 25),
    legend.text = element_text(size = 25),       # Tamaño de las etiquetas de la leyenda
    legend.title = element_text(size = 25),        # Tamaño del título de la leyenda
    panel.border = element_blank(),  # Eliminar borde del panel
    axis.line = element_blank(),  # Eliminar líneas de los ejes
    axis.ticks = element_blank(),# Eliminar "guioncito" de los ejes
    legend.position = "top") #cambio el lugar de la leyenda



NEUMONIA_grafico_grupoetario_acumulado

#


#evolutivo NEUMONIA (grafico de linea)------
NEUMONIA_evolutivo <- NEUMONIA %>% 
  group_by(ANIO, SEMANA, SE_ANIO) %>% 
  summarize(Total = sum(CANTIDAD, na.rm = TRUE), .groups = "drop") %>% 
  mutate(ANIO_SE = paste(SEMANA, ANIO, sep = "-")) %>% 
  arrange(ANIO, SEMANA) %>% 
  mutate(ANIO_SE = factor(ANIO_SE, levels = unique(ANIO_SE))) %>% 
  group_by(ANIO) %>% 
  tidyr::complete(SEMANA = 1:max(SEMANA, na.rm = TRUE), fill = list(Total = 0)) %>% 
  ungroup() %>% 
  as.data.frame()

#grafico lineas
NEUMONIA_grafevolutivo <- NEUMONIA_evolutivo %>% 
  ggplot(aes(x = ANIO_SE, y = Total, group = 1)) +  
  geom_line(size = 0.7, color = "#4e5a79", lineend = "round") +  
  geom_point(size = 2, color = "#4e5a79", shape=16) +  #shape es para poner un círculo o cualquier otro elemento en la union de la linea. Shape 16= circulo, 17= triangulo, 8=estrellita,etc) 
  scale_x_discrete(breaks = levels(factor(NEUMONIA_evolutivo$ANIO_SE))[seq(1, length(levels(factor(NEUMONIA_evolutivo$ANIO_SE))), by = 5)]) +  
  labs(x = "SE/año",
       y = "Casos de neumonía") +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 25, angle = 90, hjust = 1),
    axis.text.y = element_text(size = 25),
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    strip.text = element_text(size = 25))  


NEUMONIA_grafevolutivo


#opcion grafico acumulado-----
NAC_evolutivo_filtrado <- NEUMONIA_evolutivo%>% 
  filter(ANIO >= 2022)



NAC_evolutivo_filtrado <- NAC_evolutivo_filtrado %>%
  mutate(ANIO = as.factor(ANIO))

# Crear el gráfico
NAC_grafico_anios_V3 <- NAC_evolutivo_filtrado %>% 
  ggplot(aes(x = SEMANA, y = Total, fill = ANIO)) +
  geom_area(alpha = 0.7, position = "identity", color = "black") +
  scale_x_continuous(breaks = seq(min(NAC_evolutivo_filtrado$SEMANA),
                                  max(NAC_evolutivo_filtrado$SEMANA), by = 3))+
  scale_fill_manual(values = c(
    "2025" = "#4e5a79",
    "2024" = "#7f678f",
    "2023" = "#e0848c",
    "2022" = "#f9a17b")) +
  labs(x = "SE",
       y = "Casos de neumonía",
       fill = "Año") +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    strip.text = element_text(size = 20),
    legend.text = element_text(size = 20),       # Tamaño de las etiquetas de la leyenda
    legend.title = element_text(size = 20))        # Tamaño del título de la leyenda



NAC_grafico_anios_V3






