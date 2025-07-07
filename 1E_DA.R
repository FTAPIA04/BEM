#DIARREAS----
diarreas <- agrupada %>% 
  filter(ID_SNVS_EVENTO_AGRP==11)

ANIO_max

#set de indicadores-----

#tabla con el total y la variacion porcentual 

#selecciono solo las SE del BEM para hacer mi objeto del total de las DA de este BEM

DA_cantidad_SE_BEM <- diarreas %>%
  filter(ANIO == ANIO_max, SEMANA   %in% c(SE_BEM)) %>%
  summarise(total_Cantidad = sum(CANTIDAD, na.rm = TRUE))




#selecciono las SE del año pasado (es para hacer la variacion porcentual del set de indicadores)
DA_cantidad_SE_BEM_anioanterior <- diarreas %>%
  filter(ANIO == anio_anterior, SEMANA %in% c(SE_BEM)) %>% 
  summarise(total_Cantidad = sum(CANTIDAD, na.rm = TRUE)) 


#variacion porcentual 
DA_variacion_porcentual <- 
  round((DA_cantidad_SE_BEM - DA_cantidad_SE_BEM_anioanterior)/DA_cantidad_SE_BEM_anioanterior*100,1)

#SET INDICADORES- recuadro propiamente dicho
# Esto es para que no tenga puntos el numero
DA_cantidad_SE_BEM <- gsub("\\.", "", as.character(DA_cantidad_SE_BEM))

# Convertir el valor numérico de la variación a texto con el signo %
DA_variacion_porcentual_texto <- paste0(DA_variacion_porcentual, "%")

#  Crear el diseño del recuadro
indicador_diarreas <- ggplot() +
  # Fondo superior (naranja)
  annotate("rect", xmin = 0, xmax = 1, ymin = 0.66, ymax = 1, fill = "#FF6F31", alpha = 0.9) +
  # Fondo central (blanco)
  annotate("rect", xmin = 0, xmax = 1, ymin = 0.33, ymax = 0.66, fill = "#faf7ff", alpha = 0.8) +
  # Fondo inferior (naranja claro)
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 0.33, fill = "#FFD9C4", alpha = 0.9) +
  # Título en la parte superior
  annotate("text", x = 0.5, y = 0.83, label = "Diarrea aguda", size = 10, fontface = "bold", color = "white") +
  # Número grande en el centro
  annotate("text", x = 0.5, y = 0.495, label = DA_cantidad_SE_BEM, size = 10, fontface = "bold", color = "#FF4F00") +
  # Variación porcentual en la banda inferior
  annotate("text", x = 0.1, y = 0.165, label = "Variación", size = 8, fontface = "bold", color = "#FF6F31", hjust = 0) +
  annotate("text", x = 0.9, y = 0.165, label = DA_variacion_porcentual_texto, size = 8, fontface = "bold", color = "#FF6F31", hjust = 1) +
  theme_void()

indicador_diarreas


#DA evolutivo----------- 
#ARMO UNA TABLA 
  DA_evolutivo <- diarreas %>% 
    group_by(ANIO, SEMANA) %>% 
    summarize(Total = sum(CANTIDAD, na.rm = TRUE), .groups = "drop") %>% 
    mutate(ANIO_SE = paste(SEMANA, ANIO, sep = "-")) %>% 
    arrange(ANIO, SEMANA) %>% 
    mutate(ANIO_SE = factor(ANIO_SE, levels = unique(ANIO_SE))) %>% 
    group_by(ANIO) %>% 
    tidyr::complete(SEMANA = 1:max(SEMANA, na.rm = TRUE), fill = list(Total = 0)) %>% 
    ungroup() %>% 
    as.data.frame()
  
  
  #por qué lo convierto a factor? Mostrar imagen. Si es un factor permite el orden cronologico
  
  #objeto total de diarreas histórico
  DA_total_evolutivo <- diarreas %>% 
    summarise(Total = sum(CANTIDAD))
  
  
  #gráfico DA_evolutivo
  grafico_DA_evolutivo <- DA_evolutivo %>% 
    ggplot(aes(x = ANIO_SE, y = Total)) +
    geom_bar(stat = "identity", fill = "orange",color = "#FF8C00", width = 0.5) +
    scale_x_discrete(
      breaks = levels(DA_evolutivo$ANIO_SE)[seq(1, length(levels(DA_evolutivo$ANIO_SE)), by = 4)],
      expand = c(0, 0)) +
    scale_y_continuous(
      breaks = seq(0, max(DA_evolutivo$Total, na.rm = TRUE), by=100),
      expand = c(0, 0))+
    labs(
      x = "SE-año",
      y = "Casos de diarrea aguda") +
    theme_classic () +
    theme(
      axis.title = element_text(size = 15),
      axis.text.x = element_text(size = 12, angle = 90, hjust = 1),
      axis.text.y = element_text(size = 12),  # Fuente para los textos del eje Y
      panel.border = element_blank(),  # Eliminar borde del panel
      axis.line = element_blank(),  # Eliminar líneas de los ejes
      axis.ticks = element_blank())  # Eliminar "guioncito" de los ejes
  
  grafico_DA_evolutivo
  


#DA por grupos etarios acumulado-------------
#Armar una tabla para hacer un graf de DA por grupos etarios acumulado
  DA_tabla_grupoetario_acumulado <- diarreas %>% 
    group_by(ANIO, SEMANA, GRUPO) %>%  
    summarize(Total = sum(CANTIDAD, na.rm = TRUE), .groups = "drop") %>%  # Sumar por grupo etario
    mutate(ANIO_SE = paste(SEMANA, ANIO, sep = "-")) %>% 
    arrange(ANIO, SEMANA) %>% 
    mutate(ANIO_SE = factor(ANIO_SE, levels = unique(ANIO_SE))) %>% 
    tidyr::complete(SEMANA = 1:max(SEMANA, na.rm = TRUE), fill = list(Total = 0)) %>%
    as.data.frame()
  
  # Ver la tabla y chequeo para ver que esté ok
  
  #agrupo segun grupos etarios del BEM 
  DA_tabla_grupoetario_acumulado <- DA_tabla_grupoetario_acumulado %>%
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
  
  
  DA_tabla_grupoetario_acumulado$GRUPO_2 <- factor(DA_tabla_grupoetario_acumulado$GRUPO_2, 
                                                   levels = c("< 15 años","15 a 24 años",
                                                              "25 a 44 años","45 a 64 años", 
                                                              "65 años y más","Sin especificar"))
  
  
  
  # Crear gráfico de columnas apiladas
  DA_grafico_grupoetario_acumulado <- DA_tabla_grupoetario_acumulado %>% 
  #  filter(!is.na(GRUPO_2)) %>%  # Eliminar filas con NA en GRUPO_2
    ggplot(aes(x = ANIO_SE, y = Total, fill = GRUPO_2)) +
    geom_bar(stat = "identity", position = "fill") + #position fill es para apilado al 100%
    scale_x_discrete(
      breaks = levels(DA_tabla_grupoetario_acumulado$ANIO_SE)[seq(1, length(levels(DA_tabla_grupoetario_acumulado$ANIO_SE)), by = 5)],
      expand = c(0, 0)) +
    scale_y_continuous(
      labels = scales::percent,  # Cambiar el formato de las etiquetas a porcentaje
      breaks = seq(0, 1, by = 0.1))+ # Poner los breaks del eje Y entre 0 y 1, con intervalos de 0.1
    scale_fill_manual(values = c(
      "< 15 años" = "#ff8c00",
      "15 a 24 años" = "#be9500",  
      "25 a 44 años" = "#7a9500",  
      "45 a 64 años" = "#218f06", 
      "65 años y más" = "#00833c",
      "Sin especificar"= "#34623F")) +  
    labs(
      x = "SE-año",
      y = "% de casos de diarrea aguda",
      fill = "Grupo de edad") +
    theme_classic() +
    theme(
      axis.text.x = element_text(size = 25, angle = 90, hjust = 1),  # Rotar etiquetas en X
      axis.text.y = element_text(size = 25),  # Fuente para los textos del eje Y
      axis.title.x = element_text(size = 25),
      axis.title.y = element_text(size = 25),
      strip.text = element_text(size = 25),
      legend.text = element_text(size = 27),       # Tamaño de las etiquetas de la leyenda
      legend.title = element_text(size = 27),        # Tamaño del título de la leyenda
      panel.border = element_blank(),  # Eliminar borde del panel
      axis.line = element_blank(),  # Eliminar líneas de los ejes
      axis.ticks = element_blank(),# Eliminar "guioncito" de los ejes
      legend.position = "top") #cambio el lugar de la leyenda
      

  
  DA_grafico_grupoetario_acumulado


#DA por grupos etarios SE del BEM------------
  

#Armar una tabla para hacer un graf de DA por grupos etarios SE del BEM
  DA_tabla_grupoetario <- diarreas %>%
    filter(ANIO == ANIO_max, SEMANA %in% c(SE_BEM)) %>% 
    group_by(GRUPO) %>% 
    summarize(Total = sum(CANTIDAD, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      GRUPO_2 = case_when(
        GRUPO == "< 6 m"~ "< 6 meses",
        GRUPO == "6 a 11 m"~ "6 a 11 meses",
        GRUPO == "12 a 23 m"~ "12 a 23 meses",
        GRUPO == "2 a 4"~ "2 a 4 años",
        GRUPO == "5 a 9"~ "5 a 9 años",
        GRUPO == "10 a 14"~ "10 a 14 años",
        GRUPO == "15 a 19"~ "15 a 19 años",
        GRUPO == "20 a 24"~ "20 a 24 años",
        GRUPO == "25 a 34"~ "25 a 34 años",
        GRUPO == "35 a 44"~ "35 a 44 años",
        GRUPO == "45 a 64"~ "45 a 64 años",
        GRUPO == "65 a 74"~ "65 a 74 años",
        GRUPO == ">= a 75"~ ">= a 75 años",
        GRUPO == "Edad Sin Esp."~ "Sin especificar",
        TRUE ~ GRUPO)) 
  
  
  
  
  DA_tabla_grupoetario <- as.data.frame(DA_tabla_grupoetario)
  
  #hago la columna GRUPO2 factor para poder ordenarlo
  
  DA_tabla_grupoetario$GRUPO_2 <- factor(DA_tabla_grupoetario$GRUPO_2, 
                                         levels = c("< 6 meses","6 a 11 meses",
                                                    "12 a 23 meses","2 a 4 años", "5 a 9 años",
                                                    "10 a 14 años", "15 a 19 años",
                                                    "20 a 24 años", "25 a 34 años", "35 a 44 años",
                                                    "45 a 64 años","65 a 74 años",">= a 75 años",
                                                     "Sin especificar"))
  
  #gráfico DA grupo de edad
  DA_grafico_grupoetario <- DA_tabla_grupoetario %>% 
    ggplot(aes(x=Total, y=GRUPO_2))+
    geom_bar(stat = "identity", fill = "orange", width = 0.5)+
    geom_text(aes(label = Total), position = position_dodge(width = 0.5), hjust = - 0.2, size = 2.5) +  # Añadir etiquetas de datos a la barra
    labs(
      x = "Casos de diarrea aguda",
      y = "Grupos de edad") +
    scale_x_continuous(breaks = seq(0, max(DA_tabla_grupoetario$Total), by = 50)) +
    theme_classic () +
    theme(
      axis.text.x = element_text(size = 6),
      axis.text.y = element_text(size = 6), 
      axis.title.x = element_text(size = 6),  # Cambia el tamaño del título del eje X
      axis.title.y = element_text(size = 6),# Fuente para los textos del eje Y
      panel.border = element_blank(),  # Eliminar borde del panel
      axis.line = element_blank(),  # Eliminar líneas de los ejes
      axis.ticks = element_blank())  # Eliminar "guioncito" de los ejes
  DA_grafico_grupoetario 

#DA por regiones segun grupo de edad---------
  
  
  #Armar una tabla 
  DA_tabla_regiones_grupoetario <- diarreas %>%
    filter(ANIO == ANIO_max, SEMANA %in% c(SE_BEM)) %>% 
    group_by(GRUPO, REGION) %>%
    summarize(Total = sum(CANTIDAD, na.rm = TRUE), .groups = "drop")
  
  #agrupo segun grupos etarios del BEM 
  DA_tabla_regiones_grupoetario <- DA_tabla_regiones_grupoetario %>%
    mutate(
      GRUPO_2 = case_when(
        GRUPO == "< 6 m"~ "0 a 4 años",
        GRUPO == "6 a 11 m"~ "0 a 4 años",
        GRUPO == "12 a 23 m"~ "0 a 4 años",
        GRUPO == "2 a 4"~ "0 a 4 años",
        GRUPO == "5 a 9"~ "5 a 9 años",
        GRUPO == "10 a 14"~ "10 a 19 años",
        GRUPO == "15 a 19"~ "10 a 19 años",
        GRUPO == "20 a 24"~ "20 a 44 años",
        GRUPO == "25 a 34"~ "20 a 44 años",
        GRUPO == "35 a 44"~ "20 a 44 años",
        GRUPO == "45 a 54"~ "45 a 64 años",
        GRUPO == "45 a 64"~ "45 a 64 años",
        GRUPO == "55 a 64"~ "45 a 64 años",
        GRUPO == "65 a 74"~ "65 años y más",
        GRUPO == ">= a 75"~ "65 años y más",
        GRUPO == "Edad Sin Esp."~ "Sin especificar",
        TRUE ~ GRUPO))
  
  DA_tabla_regiones_grupoetario$GRUPO_2 <- factor(DA_tabla_regiones_grupoetario$GRUPO_2, 
                                                  levels = c("0 a 4 años","5 a 9 años",
                                                             "10 a 19 años","20 a 44 años", "45 a 64 años",
                                                             "65 años y más","Sin especificar"))
  
  #grafico por regiones segun grupo etario
  
  DA_grafico_regiones <- 
    DA_tabla_regiones_grupoetario %>% 
    ggplot(aes(x=Total, y=GRUPO_2))+
    geom_bar(stat = "identity", fill = "orange", width = 0.5) +  
    facet_wrap(~ REGION, ncol=3) +  # Facetear por la columna REGIONES
    labs(
      x = "Casos de diarrea aguda",
      y = "Grupos de edad") +
    theme_classic() +
    theme(
      axis.text.x = element_text(size = 7),
      axis.text.y = element_text(size = 7),
      axis.title.x = element_text(size = 7),
      axis.title.y = element_text(size = 7))
  
  DA_grafico_regiones 
      
