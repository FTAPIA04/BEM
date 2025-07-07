#Filtrado de agrupada con ID inmunoprevenibles####
IP <- agrupada %>% 
  filter(IDEVENTOAGRUPADO==5)

#Grafico evolutivo-----
  IP_evolutivo <- IP %>% 
    group_by(ANIO, SEMANA, NOMBREEVENTOAGRP) %>% 
    summarize(Total = sum(CANTIDAD, na.rm = TRUE), .groups = "drop") %>% 
    mutate(ANIO_SE = paste(SEMANA, ANIO, sep = "-")) %>% 
    arrange(ANIO, SEMANA) %>% 
    mutate(ANIO_SE = factor(ANIO_SE, levels = unique(ANIO_SE))) %>% 
    as.data.frame()
  
  
  #objeto sumatoria cantidad tabla evolutivo
  IP_total<- IP_evolutivo %>% 
    summarize(Total_Cantidad = sum(Total, na.rm = TRUE))
  

#armo objeto total de varicelas SE BEM

  varicela_SEBEM <- IP %>%
    filter(ID_SNVS_EVENTO_AGRP == 6, 
           ANIO == ANIO_max,  
           SEMANA %in% SE_BEM) %>% 
    summarize(Total_Cantidad = sum(CANTIDAD, na.rm = TRUE))


# Variacion porcentual

  #selecciono las SE del año pasado (es para hacer la variacion porcentual del set de indicadores)
  varicela_SEBEManterior <- IP %>%
    filter(ID_SNVS_EVENTO_AGRP == 6, 
           ANIO == anio_anterior,  
           SEMANA %in% SE_BEM) %>% 
    summarize(Total_Cantidad = sum(CANTIDAD, na.rm = TRUE))
  
  
  #variacion porcentual
  varicela_variacion_porcentual <- 
    round((varicela_SEBEM - varicela_SEBEManterior)/varicela_SEBEManterior*100,1)


#Indicador-----

# Convertir el valor numérico de la variación a texto con el signo %
  varicela_variacion_porcentual_texto <- paste0(varicela_variacion_porcentual, "%")
  
  #  Crear el diseño del recuadro
  indicador_varicela <- ggplot() +
    # Fondo superior (celeste mas oscuro) 
    annotate("rect", xmin = 0, xmax = 1, ymin = 0.66, ymax = 1, fill = "#23C3CF", alpha = 0.9) +
    # Fondo central (blanco)
    annotate("rect", xmin = 0, xmax = 1, ymin = 0.33, ymax = 0.66, fill = "#faf7ff", alpha = 0.8) +
    # Fondo inferior (celeste claro)
    annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 0.33, fill = "#93e5eb", alpha = 0.9) +
    # Título en la parte superior
    annotate("text", x = 0.5, y = 0.83, label = "Varicela", size = 10, fontface = "bold", color = "white") +
    # Número grande en el centro
    annotate("text", x = 0.5, y = 0.495, label = varicela_SEBEM, size = 10, fontface = "bold", color = "#23C3CF") +
    # Variación porcentual en la banda inferior
    annotate("text", x = 0.1, y = 0.165, label = "Variación", size = 8, fontface = "bold", color = "#23C3CF", hjust = 0) +
    annotate("text", x = 0.9, y = 0.165, label = varicela_variacion_porcentual_texto, size = 8, fontface = "bold", color = "#23C3CF", hjust = 1) +
    theme_void()
  
  indicador_varicela


#grafico evolutivo---- 

    IP_grafico_evolutivo <- IP_evolutivo %>% 
    ggplot(aes(x = ANIO_SE, y = Total, fill=NOMBREEVENTOAGRP)) +
    geom_bar(stat = "identity", width = 0.5) +
    scale_x_discrete(
      breaks = levels(IP_evolutivo$ANIO_SE)[seq(1, length(levels(IP_evolutivo$ANIO_SE)), by = 4)],
      expand = c(0, 0)) +  
    scale_y_continuous(
      breaks = seq(0, max(IP_evolutivo$Total, na.rm = TRUE), by = 5),  
      expand = c(0, 0))+
    scale_fill_manual(
      values = c("Parotiditis" = "#f9f871",  
                 "Varicela" = "#63ddb0"))+
    labs(
      x = "SE-año",
      y = "Casos de infecciones inmunoprevenibles",
      fill = "Evento") +
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
      legend.position = "right") #cambio el lugar de la leyenda
  IP_grafico_evolutivo


#Grafico varicela por grupos etarios----

  #armo tabla filtrando solo varicela
  
  Varicela_evolutivo <- IP %>% 
    filter(ID_SNVS_EVENTO_AGRP==6, IDEDAD %in% c(1, 2, 3, 4, 5, 6)) %>% 
    group_by(ANIO, SEMANA, GRUPO) %>% 
    summarize(Total = sum(CANTIDAD, na.rm = TRUE), .groups = "drop") %>% 
    mutate(ANIO_SE = paste(SEMANA, ANIO, sep = "-")) %>% 
    arrange(ANIO, SEMANA) %>% 
    mutate(ANIO_SE = factor(ANIO_SE, levels = unique(ANIO_SE))) %>% 
    as.data.frame()
  
  
  
  #armo los grupos de edad segun publicaciones del BEM 
  Varicela_evolutivo <- Varicela_evolutivo %>% 
    mutate(
      GRUPO_2 = case_when(
        GRUPO == "< 6 m"~ "Menores de 5 años",
        GRUPO == "6 a 11 m"~ "Menores de 5 años",
        GRUPO == "12 a 23 m"~ "Menores de 5 años",
        GRUPO == "2 a 4"~ "Menores de 5 años",
        GRUPO == "5 a 9"~ "5-9 años",
        GRUPO == "10 a 14"~ "10-14 años",
        TRUE ~ GRUPO))
  
  #objeto N para usarlo en titulo
  Varicela_evolutivo_total <- Varicela_evolutivo %>% 
    summarise(total_Cantidad = sum(Total, na.rm = TRUE))
  
  
  #grafico grupos de edad columnas apiladas al 100%
  
  IP_grafico_grupoetario_acumulado <- Varicela_evolutivo %>% 
    ggplot(aes(x = ANIO_SE, y = Total, fill = GRUPO_2)) +
    geom_bar(stat = "identity", position = "fill") + #position fill es para apilado al 100%
    scale_x_discrete(
      breaks = levels(Varicela_evolutivo$ANIO_SE)[seq(1, length(levels(Varicela_evolutivo$ANIO_SE)), by = 5)],
      expand = c(0, 0)) +
    scale_y_continuous(
      labels = scales::percent,  # Cambiar el formato de las etiquetas a porcentaje
      breaks = seq(0, 1, by = 0.1))+ # Poner los breaks del eje Y entre 0 y 1, con intervalos de 0.1
    scale_fill_manual(values = c(
      "Menores de 5 años" = "#63ddbd",
      "5-9 años" = "#f9f871",  
      "10-14 años" = "#008469")) +  
    labs(
      x = "SE-año",
      y = "% de casos de varicela",
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
  
  IP_grafico_grupoetario_acumulado
  

#gráfico varicela grupo de edad SEBEM-----
  
  #tabla para el grafico edades segun SE BEM
  
  varicela_tabla_grupoetario <- IP %>% 
    filter(ID_SNVS_EVENTO_AGRP==6,ANIO == ANIO_max & SEMANA %in% SE_BEM ) %>% 
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
        TRUE ~ GRUPO)) %>% 
    mutate(GRUPO_2 = factor(GRUPO_2, levels = c("< 6 meses", "6 a 11 meses", "12 a 23 meses",
                                                "2 a 4 años", "5 a 9 años", "10 a 14 años",
                                                "15 a 19 años", "20 a 24 años","25 a 34 años",
                                                "35 a 44 años","45 a 64 años", "65 a 74 años", ">= a 75 años"))) %>% 
    as.data.frame()
  
  
  #objeto total varicelas SE BEM
  
  #gráfico varicela grupo de edad
  varicela_grafico_grupoetario <- varicela_tabla_grupoetario %>% 
    ggplot(aes(x=Total, y=GRUPO_2))+
    geom_bar(stat = "identity", fill = "#63ddbd", width = 0.5)+
    geom_text(aes(label = Total), position = position_dodge(width = 0.5), hjust = - 0.2, size = 2.5) +  # Añadir etiquetas de datos a la barra
    labs(
      x = "Casos de varicela",
      y = "Grupos de edad") +
    theme_classic () +
    theme(
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10), 
      axis.title.x = element_text(size = 10),  # Cambia el tamaño del título del eje X
      axis.title.y = element_text(size = 10),# Fuente para los textos del eje Y
      panel.border = element_blank(),  # Eliminar borde del panel
      axis.line = element_blank(),  # Eliminar líneas de los ejes
      axis.ticks = element_blank())  # Eliminar "guioncito" de los ejes
  varicela_grafico_grupoetario
  

#grafico incidencia por REGION------

  varicela_incidencia <-  IP %>%
    filter(ID_SNVS_EVENTO_AGRP == 6) %>% 
    group_by(REGION, ANIO) %>%
    summarise(
      total_cantidad = sum(CANTIDAD), 
      .groups = "drop")
  
  
  
  #agrego la columna poblacion con leftjoin
  
  poblacion <- read_excel("Poblacion.xlsx")
  
  varicela_incidencia <- varicela_incidencia %>%
    left_join(poblacion, by = c("ANIO", "REGION"))
  
  
  #agrego el calculo de incidencia 
  varicela_incidencia <- varicela_incidencia %>%
    mutate(
      total_cantidad = as.numeric(total_cantidad),  # cambio formato
      poblacion = as.numeric(poblacion),  # cambio formato
      incidencia = ifelse(!is.na(total_cantidad) & !is.na(poblacion), 
                          round((total_cantidad / poblacion) * 10000, 1), 
                          NA))
  
  
  #grafico incidencia
  varicela_incidencia_grafico <- varicela_incidencia %>% 
    filter(!is.na(incidencia)) %>%  # Filtrar para excluir NA en incidencia
    ggplot(aes(x = factor(ANIO), y = incidencia)) +  # Años en el eje Y, ordenados cronológicamente
    geom_col(stat = "identity", fill = "#63ddb0", width = 0.5) +  # Gráfico de barras horizontales
    geom_text(aes(label = round(incidencia, 1)),  # Etiquetas con valores redondeados
              hjust = -0.2,  # Desplaza las etiquetas a la derecha
              size = 1.8) +  # Tamaño del texto de las etiquetas
    facet_wrap(~ REGION, ncol = 3, scales = "free_x") +  # Facetear por región, 2 columnas
    labs(
      x = "Año",
      y = "Incidencia acumulada cada 10000 habitantes") +
    theme_classic() +
    theme(
      axis.text.x = element_text(size = 7),
      axis.text.y = element_text(size = 7),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      strip.text = element_text(size = 7))  # Tamaño del texto en los facetes
  
  varicela_incidencia_grafico
