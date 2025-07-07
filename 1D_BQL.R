#Filtrado agrupada ID bql###

BQL <- agrupada %>% 
  filter(ID_SNVS_EVENTO_AGRP %in% c(7,443))

#objeto cantidad de BQL notificadas en las SE del BEM-----
BQL_cantidad_SE_BEM <- BQL %>%
  filter(ANIO == ANIO_max, SEMANA   %in% c(SE_BEM)) %>%
  summarise(total_Cantidad = sum(CANTIDAD, na.rm = TRUE))


#selecciono las SE del año pasado (es para hacer la variacion porcentual del set de indicadores)-----
BQL_cantidad_SE_BEM_anioanterior <- BQL %>%
  filter(ANIO == anio_anterior, SEMANA %in% c(SE_BEM)) %>% 
  summarise(total_Cantidad = sum(CANTIDAD, na.rm = TRUE)) 


#variacion porcentual 
BQL_variacion_porcentual <- 
  round((BQL_cantidad_SE_BEM - BQL_cantidad_SE_BEM_anioanterior)/BQL_cantidad_SE_BEM_anioanterior*100,1)

#creo objeto de aumento o disminución
BQL_texto_variacion <- ifelse(BQL_variacion_porcentual < 0, "una disminución", "un aumento")




#suma total BQL----
BQL_total_evolutivo <- BQL %>% 
  summarise(Total = sum(CANTIDAD))



#grafico acumulado grupos etarios-------
#distribución SE por grupos etarios

#Armar una tabla para hacer un graf de BQL por grupos etarios SE del BEM
BQL_tabla_grupoetario_acumulado <- BQL %>% 
  group_by(ANIO, SEMANA, GRUPO) %>%  
  summarize(Total = sum(CANTIDAD, na.rm = TRUE), .groups = "drop") %>%  # Sumar por grupo etario
  mutate(ANIO_SE = paste(SEMANA, ANIO, sep = "-")) %>% 
  arrange(ANIO, SEMANA) %>% 
  mutate(ANIO_SE = factor(ANIO_SE, levels = unique(ANIO_SE))) %>% 
  tidyr::complete(SEMANA = 1:max(SEMANA, na.rm = TRUE), fill = list(Total = 0)) %>%
  as.data.frame()

# Ver la tabla y chequeo para ver que esté ok

#agrupo segun grupos etarios del BEM 
BQL_tabla_grupoetario_acumulado <- BQL_tabla_grupoetario_acumulado %>%
  mutate(
    GRUPO_2 = case_when(
      GRUPO == "< 6 m"~ "Menores de 6 meses",
      GRUPO == "6 a 11 m"~ "6 a 11 meses",
      GRUPO == "12 a 23 m"~ "12 a 23 meses",
      TRUE ~ GRUPO))


BQL_tabla_grupoetario_acumulado <- BQL_tabla_grupoetario_acumulado %>% 
  mutate(GRUPO_2 = factor(GRUPO_2, levels = rev(c(
    "Menores de 6 meses",
    "6 a 11 meses",  
    "12 a 23 meses"))))



# Crear gráfico de columnas apiladas
BQL_grafico_grupoetario_acumulado <- BQL_tabla_grupoetario_acumulado %>% 
  #  filter(!is.na(GRUPO_2)) %>%  # Eliminar filas con NA en GRUPO_2
  ggplot(aes(x = ANIO_SE, y = Total, fill = GRUPO_2)) +
  geom_bar(stat = "identity", position = "fill") + #position fill es para apilado al 100%
  scale_x_discrete(
    breaks = levels(BQL_tabla_grupoetario_acumulado$ANIO_SE)[seq(1, length(levels(BQL_tabla_grupoetario_acumulado$ANIO_SE)), by = 5)],
    expand = c(0, 0)) +
  scale_y_continuous(
    labels = scales::percent,  # Cambiar el formato de las BQLquetas a porcentaje
    breaks = seq(0, 1, by = 0.1))+ # Poner los breaks del eje Y entre 0 y 1, con intervalos de 0.1
  scale_fill_manual(values = c(
  "Menores de 6 meses" = "#63dda5",
  "6 a 11 meses" = "#abee87",  
  "12 a 23 meses" = "#f9f871")) +  
  labs(
    x = "SE-año",
    y = "% de casos de BQL",
    fill = "Grupo de edad") +
  theme_classic() +
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

BQL_grafico_grupoetario_acumulado




#evolutivo BQL (grafico de linea)------
BQL_evolutivo <- BQL %>% 
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
BQL_grafevolutivo <- BQL_evolutivo %>% 
  ggplot(aes(x = ANIO_SE, y = Total, group = 1)) +  
  geom_line(size = 0.7, color = "#63dda5", lineend = "round") +  
  geom_point(size = 2, color = "#63dda5", shape=16) +  #shape es para poner un círculo o cualquier otro elemento en la union de la linea. Shape 16= circulo, 17= triangulo, 8=estrellita,etc) 
  scale_x_discrete(breaks = levels(factor(BQL_evolutivo$ANIO_SE))[seq(1, length(levels(factor(BQL_evolutivo$ANIO_SE))), by = 5)]) +  
  labs(x = "SE/año",
       y = "Casos de bronquiolitis") +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 25, angle = 90, hjust = 1),
    axis.text.y = element_text(size = 25),
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    strip.text = element_text(size = 25))  


BQL_grafevolutivo


#opcion grafico acumulado-----

BQL_evolutivo_filtrado <- BQL_evolutivo %>% 
  filter(ANIO >= 2022)



BQL_evolutivo_filtrado <- BQL_evolutivo_filtrado %>%
  mutate(ANIO = as.factor(ANIO))

# Crear el gráfico
BQL_grafico_anios_V3 <- BQL_evolutivo_filtrado %>% 
  ggplot(aes(x = SEMANA, y = Total, fill = ANIO)) +
  geom_area(alpha = 0.7, position = "identity", color = "black") +
  scale_x_continuous(breaks = seq(min(BQL_evolutivo_filtrado$SEMANA),
                                  max(BQL_evolutivo_filtrado$SEMANA), by = 3))+
  scale_fill_manual(values = c(
    "2025" = "#E9FF70",
    "2024" = "#FFD670",
    "2023" = "#FF70A6",
    "2022" = "#70D6FF")) +
  labs(x = "SE",
       y = "Casos de bronquiolitis",
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



BQL_grafico_anios_V3


