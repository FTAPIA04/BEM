#armo la base
VRNOMINAL_EVENTOCASO2 <- as.data.frame(VRNOMINAL_EVENTOCASO)


# Filtrar los casos positivos (determinacion_dico==1)
VRNOMINAL_EVENTOCASO2 <- as.data.frame(VRNOMINAL_EVENTOCASO) %>% 
  filter(DETERMINACION_DICO == 1) %>% 
  filter(AÑO >= 2024)


#filtro segun SE y año del BEM

VRNOMINAL_EVENTOCASO2 <- VRNOMINAL_EVENTOCASO2 %>% 
  filter(AÑO < AÑO_BEM_IRAG | (AÑO == AÑO_BEM_IRAG & SEPI_ <= max_SE_IRAG))



# 2. Seleccionamos SEPI_, AÑO y SOLO las determinaciones que nos interesan
determinaciones_interes <- c(
  "DETERMINACION_Genoma viral de VSR",
  "DETERMINACION_Genoma viral SARS-CoV-2",
  "DETERMINACION_Genoma viral de Parainfluenza 1",
  "DETERMINACION_Genoma viral de Metaneumovirus Humano",
  "DETERMINACION_Genoma viral de Influenza A (sin subtipificar)",
  "DETERMINACION_Genoma viral de Parainfluenza 4",                                      
  "DETERMINACION_Detección molecular de Adenovirus",                                   
  "DETERMINACION_Genoma viral de Parainfluenza 3",                                      
  "DETERMINACION_Genoma viral de Influenza B (sin linaje)",                            
  "DETERMINACION_Genoma viral de Influenza A H3N2",                                     
  "DETERMINACION_Genoma viral de Parainfluenza 2",                                      
  "DETERMINACION_Genoma viral de Adenovirus",                                           
  "DETERMINACION_Genoma viral de Influenza A H1N1pdm",                                 
  "DETERMINACION_Genoma viral de VSR B",                                                
  "DETERMINACION_Genoma viral de Influenza B, linaje Victoria",                         
  "DETERMINACION_Antígeno viral de VSR",                                                
  "DETERMINACION_Genoma viral de VSR A",                                                
  "DETERMINACION_Antígenos virales por panel de 8 virus",                               
  "DETERMINACION_Detección de Antígeno de SARS CoV-2",                                  
  "DETERMINACION_Genoma viral de Influenza",                                            
  "DETERMINACION_Antigeno viral Metaneumovirus Humano",                                 
  "DETERMINACION_Antígeno viral de influenza A",                                        
  "DETERMINACION_Antígeno viral de influenza B")                                       

  
#armo tabla 
tabla_determinaciones <- VRNOMINAL_EVENTOCASO2 %>%
  select(SEPI_, AÑO, any_of(determinaciones_interes))

# 3. Pivot a formato largo
tabla_larga <- tabla_determinaciones %>%
  pivot_longer(
    cols = all_of(determinaciones_interes),
    names_to = "Determinacion",
    values_to = "Valor" )

# 4. Filtramos solo los valores "Detectable"
tabla_filtrada <- tabla_larga %>%
  filter(Valor == "Detectable")

# 5. Agrupamos y contamos
tabla_resumen <- tabla_filtrada %>%
  group_by(SEPI_, AÑO, Determinacion) %>%
  summarise(Cantidad = n(), .groups = "drop")




#mutate para agrupar 

tabla_resumen_agrupada <- tabla_resumen %>%
  mutate(
    Determinacion2 = case_when(
      str_detect(Determinacion, regex("adenovirus", ignore_case = TRUE)) ~ "Adenovirus",
      str_detect(Determinacion, regex("SARS-CoV-2", ignore_case = TRUE)) ~ "SARS-CoV-2",
      str_detect(Determinacion, regex("Parainfluenza", ignore_case = TRUE)) ~ "Parainfluenza",
      str_detect(Determinacion, regex("Influenza", ignore_case = TRUE)) ~ "Influenza",
      str_detect(Determinacion, regex("VSR", ignore_case = TRUE)) ~ "VSR",
      str_detect(Determinacion, regex("Metaneumovirus", ignore_case = TRUE)) ~ "Metaneumovirus",
      TRUE ~ Determinacion))



#armo SE-año
tabla_resumen_agrupada <- tabla_resumen_agrupada %>%
  mutate(SE_año = paste(SEPI_, AÑO, sep = "-"))

#columna SE-año como factor (es para ordenarlo despues en el grafico)
tabla_resumen_agrupada <- tabla_resumen_agrupada %>%
  mutate(SE_año = factor(SE_año, levels = unique(SE_año)))

  
tabla_resumen_agrupada <- tabla_resumen_agrupada %>%
  arrange(AÑO, SEPI_) %>%
  mutate(
    sepi_str = stringr::str_pad(SEPI_, width = 2, pad = "0"),
    SE_año = paste(sepi_str, AÑO, sep = "-"), 
    SE_año = factor(SE_año, levels = unique(SE_año))) %>%
  select(-sepi_str)

#objeto cantidad para el título
cant_vigilancia_universal <- tabla_resumen_agrupada %>% 
  summarise(Cantidad = sum(Cantidad, na.rm = TRUE))


#grafico----

niveles <- unique(tabla_resumen_agrupada$SE_año)
niveles_marcados <- niveles[seq(1, length(niveles), by = 2)]

max_y_val <- tabla_resumen_agrupada %>%
  group_by(SE_año) %>%
  summarise(total = sum(Cantidad)) %>%
  pull(total) %>%
  max()


grafico_vigilancia_universal_evolutivo <- tabla_resumen_agrupada %>%
  ggplot(aes(x = factor(SE_año, levels = niveles), y = Cantidad, fill = Determinacion2)) +
  geom_col(position = "stack") +
  scale_x_discrete(breaks = niveles_marcados) +
  scale_y_continuous(breaks = seq(0, max_y_val, by = 5)) +  # Usás `max_y_val` acá
  scale_fill_manual(values = c(
    "Adenovirus" = "#F69327",
    "Metaneumovirus" = "#F9A850",
    "SARS-CoV-2" = "#ffd65f",
    "Influenza" = "#2d9963",
    "Parainfluenza" = "#6aa84f",
    "VSR" = "#B4DA5D")) + 
  labs(
    x = "SE-año",
    y = "Cantidad",
    fill = "Determinación") +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 25, angle = 90, hjust = 1),
    axis.text.y = element_text(size = 25),
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    strip.text = element_text(size = 25),
    legend.text = element_text(size = 27),
    legend.title = element_text(size = 27),
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "top")

grafico_vigilancia_universal_evolutivo


#N para el titulo 
cant_vigilancia_universal <- tabla_resumen_agrupada %>%
  summarise(cantidad_total = sum(Cantidad)) %>%
  pull(cantidad_total)


##########################################################################
#################### INFLUENZA ###########################################
##########################################################################

# 1. Filtrar la base original por SE y AÑO del BEM
VRNOMINAL_EVENTOCASO2 <- as.data.frame(VRNOMINAL_EVENTOCASO) %>%
 filter(AÑO < AÑO_BEM_IRAG | (AÑO == AÑO_BEM_IRAG & SEPI_ <= max_SE_IRAG))

# 2. Seleccionar solo variables de interés + DETERMINACION_DICO
tabla_determinaciones <- VRNOMINAL_EVENTOCASO2 %>%
  select(IDEVENTOCASO, SEPI_, AÑO, DETERMINACION_DICO, any_of(determinaciones_interes))

# 3. Pivotear a formato largo
tabla_larga <- tabla_determinaciones %>%
  pivot_longer(
    cols = all_of(determinaciones_interes),
    names_to = "Determinacion",
    values_to = "Valor")

# 4. Filtrar valores Detectables y DETERMINACION_DICO == 1
tabla_filtrada <- tabla_larga %>%
  filter(Valor == "Detectable", DETERMINACION_DICO == 1)

# 5. Agrupar y contar 
tabla_influenza_desglosada <- tabla_resumen %>%
  filter(
    str_detect(Determinacion, regex("Influenza", ignore_case = TRUE)) &
      !str_detect(Determinacion, regex("Parainfluenza", ignore_case = TRUE))) %>%
  mutate(
    Determinacion = str_remove(Determinacion, "^DETERMINACION_"),
    Determinacion = str_remove(Determinacion, "^Genoma viral de")) %>%
  arrange(AÑO, SEPI_) %>%
  mutate(
    SE_año = paste0(str_pad(SEPI_, 2, pad = "0"), "-", AÑO),
    SE_año = factor(SE_año, levels = unique(paste0(str_pad(SEPI_, 2, pad = "0"), "-", AÑO))))



#N para el titulo 
cant_influenza <- tabla_influenza_desglosada %>%
 summarise(cantidad_total = sum(Cantidad)) %>%
 pull(cantidad_total)


#grafico
grafico_influenza <- tabla_influenza_desglosada %>% 
ggplot(aes(x = SE_año, y = Cantidad, fill = Determinacion)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    x = "SE-Año",
    y = "Determinaciones",
    fill= NULL) +
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
grafico_influenza 



#########################################################################
########################## % positividad ################################
#########################################################################


# positividad semanal
positividad_semanal <- VRNOMINAL_EVENTOCASO %>%
  filter(AÑO >= 2024) %>%
  filter(
    AÑO < 2025 | (AÑO == 2025 & SEPI_ <= SE_BEM_MAX)) %>% 
  select(AÑO, IDEVENTOCASO, SEPI_, starts_with("DETERMINACION_")) %>%
  pivot_longer(
    cols = starts_with("DETERMINACION_"),
    names_to = "virus",
    values_to = "resultado") %>%
  filter(!is.na(resultado)) %>%
  mutate(
    positivo = str_to_lower(resultado) %in% c("detectable", "positivo")) %>%
  group_by(AÑO, SEPI_, virus) %>%
  summarise(
    positivos = sum(positivo, na.rm = TRUE),
    total = n(),
    positividad = round((positivos / total) * 100, 1),
    .groups = "drop")

# Junto los virus en FLU, SVR y SARS-CoV-2 
positividad_semanal_filtrada <- positividad_semanal %>%
  filter(
    str_detect(virus, regex("VSR", ignore_case = TRUE)) |
      str_detect(virus, regex("SARS", ignore_case = TRUE)) |
      str_detect(virus, regex("Influenza", ignore_case = TRUE))) %>%
  mutate(
    virus = case_when(
      str_detect(virus, regex("VSR", ignore_case = TRUE)) ~ "VSR",
      str_detect(virus, regex("Influenza", ignore_case = TRUE)) ~ "Influenza",
      str_detect(virus, regex("SARS", ignore_case = TRUE)) ~ "SARS_COV_2",
      TRUE ~ "Otro")) %>%
  filter(virus != "Otro") %>%
  group_by(AÑO, SEPI_, virus) %>%  # agrupar de nuevo por categoría
  summarise(
    positivos = sum(positivos),
    total = sum(total),
    positividad = round((positivos / total) * 100, 1),
    .groups = "drop") %>%
  mutate(
    SE_año = paste0(str_pad(SEPI_, 2, pad = "0"), "-", AÑO),
    SE_año = factor(SE_año, levels = unique(SE_año)))

positividad_semanal_filtrada <- positividad_semanal_filtrada %>% 
  mutate(virus=case_when(
    virus=="SARS_COV_2"~"SARS-CoV-2",
    TRUE~virus))

# Grafico
grafico_positividad_lineas <- positividad_semanal_filtrada %>% 
  ggplot(aes(x = SE_año, y = positividad, color = virus, group = virus)) +
  geom_line(size = 0.4) + 
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "VSR" = "#B4DA5D",
    "Influenza" = "#4DB6AC",
    "SARS-CoV-2" = "#ffd65f")) +
  scale_x_discrete(
    breaks = levels(positividad_semanal_filtrada$SE_año)[
      seq(1, length(levels(positividad_semanal_filtrada$SE_año)), by = 3)
    ]) +
  labs(
    x = "Semana Año",
    y = "Positividad (%)",
    color = NULL) +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 25, angle = 90, hjust = 1),
    axis.text.y = element_text(size = 25),
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    strip.text = element_text(size = 25),
    legend.text = element_text(size = 25),
    legend.title = element_text(size = 25),
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "top",
    legend.direction = "horizontal", 
    legend.box = "horizontal")

grafico_positividad_lineas


##################################################################
##################### automatizacion texto #######################
#################################################################

# Influenza####
positividad_semanal_BEM_FLU <- positividad_semanal_filtrada %>% 
  filter(AÑO %in% AÑO_BEM_IRAG, SEPI_ %in% SE_BEM_IRAG) %>% 
  filter(virus == "Influenza") %>%
  mutate(texto_positividad = if_else(
    positividad < 1,
    "menor a 1%",
    paste0(positividad, "%")))


semana_maxima_FLU <- positividad_semanal_BEM_FLU %>%
  slice_max(order_by = positividad, n = 1)

semana_FLU <- semana_maxima_FLU$SEPI_  
porcentaje_positividad_FLU <- semana_maxima_FLU$positividad

# Verificar si todas las semanas tienen positividad < 1 para Influenza
todas_menor_a_1_FLU <- all(positividad_semanal_BEM_FLU$positividad < 1)

# Texto automatizado según condición
texto_analisis_FLU <- if (todas_menor_a_1_FLU) {
  "En las semanas de este BEM, los porcentajes de positividad para Influenza fueron menores al 1%."
} else {
  semana_maxima_FLU <- positividad_semanal_BEM_FLU %>%
    slice_max(order_by = positividad, n = 1)
  
  semana_FLU <- semana_maxima_FLU$SE_año
  porcentaje_positividad_FLU <- semana_maxima_FLU$positividad
  
  paste0(
    "En las semanas de este BEM, fue en la SE ", semana_FLU,
    " el mayor porcentaje de positividad para Influenza, siendo del ",
    porcentaje_positividad_FLU, "%."
  )
}

#mostrar texto
texto_analisis_FLU



#Influenza año actual
positividad_semanal_FLU_añoactual <- positividad_semanal_filtrada %>% 
  filter(AÑO == AÑO_BEM_IRAG) %>% 
  filter(virus == "Influenza") %>%
  mutate(texto_positividad = if_else(
    positividad < 1,
    "menor a 1%",
    paste0(positividad, "%")))



# VSR######

positividad_semanal_BEM_VSR <- positividad_semanal_filtrada %>% 
  filter(AÑO %in% AÑO_BEM_IRAG, SEPI_ %in% SE_BEM_IRAG) %>% 
  filter(virus == "VSR") %>%
  mutate(texto_positividad = if_else(
    positividad < 1,
    "menor a 1%",
    paste0(positividad, "%")))

semana_maxima_VSR<- positividad_semanal_BEM_VSR %>%
  slice_max(order_by = positividad, n = 1)

semana_VSR <- semana_maxima_VSR$SEPI_  
porcentaje_positividad_VSR <- semana_maxima_VSR$positividad

# Verificar si todas las semanas tienen positividad < 1
todas_menor_a_1 <- all(positividad_semanal_BEM_VSR$positividad < 1)

# Texto automatizado según condición
texto_analisis_VSR <- if (todas_menor_a_1) {
  "En las semanas de este BEM, los porcentajes de positividad fueron menores al 1%."
} else {
  semana_maxima_VSR <- positividad_semanal_BEM_VSR %>%
    slice_max(order_by = positividad, n = 1)
  
  semana_VSR <- semana_maxima_VSR$SE_año
  porcentaje_positividad_VSR <- semana_maxima_VSR$positividad
  
  paste0(
    "En las semanas de este BEM, fue en la SE ", semana_VSR,
    " el mayor porcentaje de positividad, siendo del ",
    porcentaje_positividad_VSR, "%."
  )
}

# Mostrar el texto
texto_analisis_VSR



#VSR año actual 
positividad_semanal_VSR_añoactual <- positividad_semanal_filtrada %>% 
  filter(AÑO == AÑO_BEM_IRAG) %>% 
  filter(virus == "VSR") %>%
  mutate(texto_positividad = if_else(
    positividad < 1,
    "menor a 1%",
    paste0(positividad, "%")))


# SARS-CoV-2 #########
positividad_semanal_BEM_COVID<- positividad_semanal_filtrada %>% 
  filter(AÑO %in% AÑO_BEM_IRAG, SEPI_ %in% SE_BEM_IRAG) %>% 
  filter(virus == "SARS-CoV-2") %>%
  mutate(texto_positividad = if_else(
    positividad < 1,
    "menor a 1%",
    paste0(positividad, "%")))


# Identificar la semana con mayor positividad
semana_maxima_COVID <- positividad_semanal_BEM_COVID %>%
  slice_max(order_by = positividad, n = 1)

# Generar texto según si todas las semanas son <1%
todas_menor_a_1_COVID <- all(positividad_semanal_BEM_COVID$positividad < 1)

texto_analisis_COVID <- if (todas_menor_a_1_COVID) {
  "En las semanas de este BEM, los porcentajes de positividad para SARS-CoV-2 fueron menores al 1%."
} else {
  paste0(
    "En las semanas de este BEM, fue en la SE ", semana_maxima_COVID$SE_año,
    " el mayor porcentaje de positividad para SARS-CoV-2, siendo del ",
    semana_maxima_COVID$positividad, "%."
  )
}

texto_analisis_COVID

