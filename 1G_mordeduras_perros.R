#filtrado de base agrupada con ID evento####
  mordedura_perro <- agrupada %>% 
    filter(ID_SNVS_EVENTO_AGRP %in% c(341, 507, 508, 509, 512))
  

#tabla mordedura evolutivo####
  
  mordeduras_evolutivo <- mordedura_perro %>%
    group_by(SEMANA, ANIO) %>%  
    summarise(total_cantidad = sum(CANTIDAD), .groups = "drop") %>%  # Suma la columna cantidad, ignorando NA
    arrange(ANIO, SEMANA) %>% 
    mutate(SE_ANIO = paste(SEMANA, ANIO, sep = "-")) %>%  # Crear la variable como texto: "ANIO-SEMANA"
    mutate(SE_ANIO = factor(SE_ANIO, levels = unique(SE_ANIO))) %>%   # Convertir a factor, asegurando el orden cronológico
    tidyr::complete(SEMANA = 1:max(SEMANA, na.rm = TRUE), fill = list(Total = 0)) 
  

#indicador############## (van a ver distintos objetos que son utilizados para la construccion del indicador)
  
#objeto n mordeduras acumulado
  mordeduras_total <- sum(mordeduras_evolutivo$total_cantidad, na.rm = TRUE)
  
  mordeduras_total <- as.character(mordeduras_total) #lo convierto para el titulo
  
  #objeto n mordeduras SE BEM
 mordeduras_cantidad_SE_BEM <- mordedura_perro %>%
    filter(ANIO == ANIO_max, SEMANA   %in% c(SE_BEM)) %>% #Es importante ir cambiando el año y las SE según corresponda al BEM
    summarise(total_Cantidad = sum(CANTIDAD, na.rm = TRUE))
  
  
  
  #objeto n mordeduras SE BEM año anterior (es para variacion %)
 mordeduras_cantidad_SE_BEManterior <- mordedura_perro %>%
 filter(ANIO == anio_anterior, SEMANA   %in% c(SE_BEM)) %>% #Es importante ir cambiando el año y las SE según corresponda al BEM
   summarise(total_Cantidad = sum(CANTIDAD, na.rm = TRUE))
 
 #variacion porcentual 
 mordedura_variacion_porcentual <- 
   round((mordeduras_cantidad_SE_BEM - mordeduras_cantidad_SE_BEManterior)/mordeduras_cantidad_SE_BEManterior*100,1)
   
   
 
#Indicadores- rectangulo principal propiamente dicho

   
#objetos para el set de indicadores
   #hago una tabla para ver los eventos de este grupo
   mordeduras_eventos <- mordedura_perro %>%
     filter(SEMANA %in% SE_BEM,                                     # Filtrar por el valor de SE_BEM
            ANIO == ANIO_max) %>%                     # Filtrar por el año máximo
     group_by(NOMBREEVENTOAGRP, ID_SNVS_EVENTO_AGRP) %>%                     # Agrupar por los eventos
     summarize(
       Total = sum(CANTIDAD, na.rm = TRUE),                # Calcular el total
       .groups = "drop")
   
  
  
#construccion de indicadores
   
   
   # Recuadro superior
   indicador_mordedura <- ggplot() +
     annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = "#c0de00", alpha = 0.9) +
     annotate("rect", xmin = 0.1, xmax = 0.9, ymin = 0.3, ymax = 0.7, fill = "white", alpha = 0.8) +
     annotate("text", x = 0.5, y = 0.8, label = "Lesiones por mordedura de perro", 
              size = 10, fontface = "bold", color = "white") +
     annotate("text", x = 0.5, y = 0.5, label = mordeduras_cantidad_SE_BEM, 
              size = 10, fontface = "bold", color = "#00a44a") +
     # Variación porcentual en la banda inferior
     annotate("text", x = 0.1, y = 0.165, label = "Variación", size = 8, fontface = "bold", color = "#00a44a", hjust = 0) +
     annotate("text", x = 0.9, y = 0.165, label = mordedura_variacion_porcentual, size = 8, fontface = "bold", color = "#00a44a", hjust = 1) +
     theme_void()
  
   
   # Mostrar el gráfico final
   indicador_mordedura
   
  
###grafico treemap  ----

#tabla
mordeduras_tipo <- mordedura_perro%>%
  filter(SEMANA %in% SE_BEM,                                     # Filtrar por el valor de SE_BEM
         ANIO == ANIO_max) %>%                     # Filtrar por el año máximo
  group_by(NOMBREEVENTOAGRP, ID_SNVS_EVENTO_AGRP) %>%                     # Agrupar por los eventos
  summarize(
    Total = sum(CANTIDAD, na.rm = TRUE),                # Calcular el total
    .groups = "drop")

#mutate para abreviar nombre
mordeduras_tipo <- mordeduras_tipo %>% 
  mutate(NOMBRE_ABREV = case_when(
    NOMBREEVENTOAGRP == "Lesiones por mordedura de perro conocido en la vía pública"~"Perro conocido en la vía pública",
    NOMBREEVENTOAGRP == "Lesiones por mordedura de perro desconocido en la vía pública" ~ "Perro desconocido en la vía pública",
    NOMBREEVENTOAGRP == "Lesiones por mordedura de perro en la vivienda" ~ "Perro en la vivienda",
    NOMBREEVENTOAGRP == "Lesiones por mordedura de perro sin especificar"~ "Sin especificar",
    TRUE ~ NOMBREEVENTOAGRP))



#grafico treemap 
mordeduras_treemap <- mordeduras_tipo %>% 
  ggplot(aes(area = Total, fill = Total, 
             label = paste(NOMBRE_ABREV, "\n", Total))) +  
  geom_treemap() +
  geom_treemap_text(colour = "white", 
                    place = "centre", #alineacion 
                    size = 10,  #tamaño de la fuente
                    reflow = TRUE, #Ajusta el texto si es muy grande
                    grow = FALSE) +
  scale_fill_gradient(low = "#c0de00", high = "#00a44a") + #el gradiente de color
  theme(legend.position = "none",
        text = element_text(family = "Montserrat"),  
        plot.title = element_text(face = "bold"))

mordeduras_treemap


###grafico evolutivo----
  mordeduras_grafico_evolutivo <- mordeduras_evolutivo %>% 
    ggplot(aes(x = SE_ANIO, y = total_cantidad)) +
    geom_bar(stat = "identity", fill = "#7dd473", width = 0.5) +
    scale_x_discrete(
      breaks = levels(mordeduras_evolutivo$SE_ANIO)[seq(1, length(levels(mordeduras_evolutivo$SE_ANIO)), by = 5)],
      expand = c(0, 0)) +  
    labs(
      x = "SE-año",
      y = "Casos de mordeduras de perros") +
    theme_classic() +
    theme(
      axis.title = element_text(size = 25),
      axis.text.x = element_text(size = 25, angle = 90, hjust = 1),
      axis.text.y = element_text(size = 25),
      panel.border = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank())
  mordeduras_grafico_evolutivo

 
#mordeduras incidencia---------

  #calculo de incidencia
  #cargo la base de poblaciones (es la misma que están usando en el BEM)
  poblacion <- read_excel("Poblacion.xlsx")
  
  #armo la tabla
  
  mordedura_incidencia <- mordedura_perro %>%
    filter(SEMANA %in% SE_BEM) %>% #estoy usando el objeto SE_BEM asique mucha atención a poner las SE correctas del BEM
    group_by(REGION, ANIO) %>%
    summarise(
      total_cantidad = sum(CANTIDAD),
      .groups = "drop")
  
  #agrego la columna poblacion con leftjoin
  mordedura_incidencia<- mordedura_incidencia %>%
    left_join(poblacion, by = c("ANIO", "REGION"))
  
  
  
  #agrego el calculo de incidencia 
  mordedura_incidencia <- mordedura_incidencia %>%
    mutate(
      total_cantidad = as.numeric(total_cantidad),  # cambio formato
      poblacion = as.numeric(poblacion),  # cambio formato
      incidencia = ifelse(!is.na(total_cantidad) & !is.na(poblacion), 
                          round((total_cantidad / poblacion) * 10000, 1), 
                          NA)) %>% 
    complete(ANIO,REGION) #BUSCAR chatgpt
  
  
  mordedura_incidencia <- as.data.frame(mordedura_incidencia)
  
  
  #grafico incidencia
  mordedura_incidencia_grafico <- mordedura_incidencia %>% 
    filter(!is.na(REGION)) %>%  # Filtrar para excluir NA en incidencia
    ggplot(aes(y = incidencia, x = factor(ANIO))) +  # Años en el eje Y, ordenados cronológicamente
    geom_col(stat = "identity", fill = "#7dd473", width = 0.5) +  # Gráfico de barras horizontales
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
  
  mordedura_incidencia_grafico

  
  
  