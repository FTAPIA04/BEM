#filtrado de base agrupada con ID de Lesiones en el hogar---------
lesiones_hogar <- agrupada %>% 
  filter(IDEVENTOAGRUPADO==116) %>% 
  filter((ANIO > 2023) | (ANIO == 2023 & SEMANA >= 21)) 

#Cantidad segun SE BEM------
lesiones_hogar_SE_BEM <- lesiones_hogar %>%
  filter(ANIO == ANIO_max, SEMANA   %in% c(SE_BEM)) %>% #Es importante ir cambiando el año y las SE según corresponda al BEM
  summarize(Total = sum(CANTIDAD, na.rm = TRUE), .groups = "drop") 



#---- indicador con N se las SE BEM -----------  
#INDICADOR

indicador_lesiones <- ggplot() +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = "#4286f5", alpha = 0.9) +
  annotate("rect", xmin = 0.1, xmax = 0.9, ymin = 0.3, ymax = 0.7, fill = "white", alpha = 0.8) +
  annotate("text", x = 0.5, y = 0.8, label = "Internaciones por lesiones en el hogar", 
           size = 8, fontface = "bold", color = "white") +
  annotate("text", x = 0.5, y = 0.5, label = lesiones_hogar_SE_BEM, size = 10, fontface = "bold", color = "#003366") +
  theme_void()

# Mostrar solo el recuadro superior
indicador_lesiones


#####grafico treemap que reemplaza el set de indicadores----------
#hago una tabla para ver los eventos de este grupo
lesiones_eventos <- lesiones_hogar %>%
  filter(SEMANA %in% SE_BEM,                                     # Filtrar por el valor de SE_BEM
         ANIO == ANIO_max) %>%                     # Filtrar por el año máximo
  group_by(NOMBREEVENTOAGRP, ID_SNVS_EVENTO_AGRP) %>%                     # Agrupar por los eventos
  summarize(
    Total = sum(CANTIDAD, na.rm = TRUE),                # Calcular el total
    .groups = "drop")

#MUY IMPORTANTE la  tabla "lesiones_eventos": define los eventos que se muestran en el set de indicadores
#"si hay alguno nuevo" hay que agregarlo al mutate



#uso mutate para abreviar los nombres de los eventos
lesiones_eventos <- lesiones_eventos %>% 
  mutate(NOMBRE_ABREV = case_when(
    NOMBREEVENTOAGRP == "Lesiones en el hogar sin especificar"~"Sin especificar",
    NOMBREEVENTOAGRP == "Lesiones por caídas y golpes"~"Caídas y golpes",
    NOMBREEVENTOAGRP == "Lesiones por cortes y quemaduras" ~ "Cortes y quemaduras",
    NOMBREEVENTOAGRP == "Otras lesiones en el hogar"~ "Otras lesiones en el hogar",
    TRUE ~ NOMBREEVENTOAGRP))


#grafico treemap 
lesiones_treemap <- lesiones_eventos %>% 
  ggplot(aes(area = Total, fill = Total, 
             label = paste0(NOMBRE_ABREV, "\n", Total))) +  
  geom_treemap() +
  geom_treemap_text(colour = "white", 
                    place = "center",  # Alineacion
                    size = 12,  # Tamaño
                    reflow = TRUE,  # Ajusta el texto si es muy grande
                    grow = FALSE) +  
  scale_fill_gradient(low = "#4286f5", high = "#003366") +  
  theme(legend.position = "none",
        text = element_text(family = "Montserrat"),  
        plot.title = element_text(face = "bold"))

lesiones_treemap


####---lesiones evolutivo---------
#tabla_lesiones_ evolutivo
lesiones_evolutivo <- lesiones_hogar %>%
  group_by(SEMANA,ANIO) %>%  
  summarize(Total = sum(CANTIDAD, na.rm = TRUE), .groups = "drop")%>%   # Suma la columna cantidad, ignorando NA
  mutate(ANIO_SE = paste(SEMANA, ANIO, sep = "-")) %>% 
  arrange(ANIO, SEMANA) %>% 
  mutate(ANIO_SE = factor(ANIO_SE, levels = unique(ANIO_SE))) %>% 
  tidyr::complete(SEMANA = 1:max(SEMANA, na.rm = TRUE), fill = list(Total = 0)) %>% 
  as.data.frame()



#objeto n lesiones
lesiones_total <- sum(lesiones_evolutivo$Total, na.rm = TRUE)

#grafico evolutivo lesiones

lesiones_grafico_evolutivo <- lesiones_evolutivo %>% 
  ggplot(aes(x = ANIO_SE,  y = Total)) +
  geom_bar(stat = "identity", fill = "#21618c", width = 0.5) +
  scale_x_discrete(
    breaks = levels(lesiones_evolutivo$ANIO_SE)[seq(1, length(levels(lesiones_evolutivo$ANIO_SE)), by =3)])+
  scale_y_continuous(
    breaks = seq(0, max(lesiones_evolutivo$Total, na.rm = TRUE), by = 5),
    expand = c(0, 0)) +
  labs(
    x = "SE-año",
    y = "Lesiones en el hogar") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 10, angle = 90, hjust = 1),
    axis.text.y = element_text(size = 10),
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank())

# Mostrar el gráfico
lesiones_grafico_evolutivo 
