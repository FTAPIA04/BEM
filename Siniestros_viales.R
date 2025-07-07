#filtrado de base agrupada con ID de siniestros viales####
siniestros_viales <- agrupada %>% 
  filter(IDEVENTOAGRUPADO == 115) %>% 
  filter((ANIO > 2023) | (ANIO == 2023 & SEMANA >= 21))


#armo la tabla evolutivo----
siniestros_viales_evolutivo <- siniestros_viales %>%
  group_by(SEMANA, ANIO) %>%  
  summarise(total_cantidad = sum(CANTIDAD), .groups = "drop") %>%  # Suma la columna cantidad, ignorando NA
  arrange(ANIO, SEMANA) %>% 
  mutate(SE_ANIO = paste(SEMANA,ANIO, sep = "-")) %>%  # Crear la variable como texto: "ANIO-SEMANA"
  mutate(SE_ANIO = factor(SE_ANIO, levels = unique(SE_ANIO))) %>%   # Convertir a factor, asegurando el orden cronológico 
  tidyr::complete(SEMANA = 1:max(SEMANA, na.rm = TRUE), fill = list(Total = 0)) 


#armo objeto total de siniestros viales----
siniestros_viales_total <- siniestros_viales_evolutivo %>% 
  summarise(total_Cantidad = sum(total_cantidad, na.rm = TRUE))

#armo  total SE BEM de siniestros viales SE BEM----
siniestros_viales_SEBEM <- siniestros_viales_evolutivo %>% 
  filter((ANIO == ANIO_max & SEMANA %in% SE_BEM)) %>%
  summarise(total_Cantidad = sum(total_cantidad, na.rm = TRUE))

#armo tabla para ver los eventos del grupo y el ID-----
siniestrov_eventos <- siniestros_viales %>%
  filter(SEMANA %in% SE_BEM,                                     # Filtrar por el valor de SE_BEM
         ANIO == ANIO_max) %>%                     # Filtrar por el año máximo
  group_by(NOMBREEVENTOAGRP, ID_SNVS_EVENTO_AGRP) %>%                     # Agrupar por los eventos
  summarize(
    Total = sum(CANTIDAD, na.rm = TRUE),                # Calcular el total
    .groups = "drop")



#INDICADOR------

# Recuadro superior
indicador_accidentes <- ggplot() +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = "#e24a38", alpha = 0.9) +
  annotate("rect", xmin = 0.1, xmax = 0.9, ymin = 0.3, ymax = 0.7, fill = "#faf7ff", alpha = 0.8) +
  annotate("text", x = 0.5, y = 0.8, label = "Internaciones por siniestros viales", 
           size = 10, fontface = "bold", color = "#faf7ff") +
  annotate("text", x = 0.5, y = 0.5, label = siniestros_viales_SEBEM, 
           size = 10, fontface = "bold", color = "#e24a38") +
  theme_void()

indicador_accidentes


#grafico treemap----
siniestrov_eventos <- siniestros_viales%>%
  filter(SEMANA %in% SE_BEM,                                     # Filtrar por el valor de SE_BEM
         ANIO == ANIO_max) %>%                     # Filtrar por el año máximo
  group_by(NOMBREEVENTOAGRP, ID_SNVS_EVENTO_AGRP) %>%                     # Agrupar por los eventos
  summarize(
    Total = sum(CANTIDAD, na.rm = TRUE),                # Calcular el total
    .groups = "drop")


#grafico treemap 
siniestrosviales_treemap <- siniestrov_eventos %>% 
  ggplot(aes(area = Total, fill = Total, 
             label = paste(NOMBREEVENTOAGRP, "\n", Total))) +  
  geom_treemap() +
  geom_treemap_text(colour = "white", 
                    place = "centre", #alineacion 
                    size = 10,  #tamaño de la fuente
                    reflow = TRUE, #Ajusta el texto si es muy grande
                    grow = FALSE) +
  scale_fill_gradient(low = "#EE9B91", high = "#e24a38") + #el gradiente de color
  theme(legend.position = "none",
        text = element_text(family = "Montserrat"),  
        plot.title = element_text(face = "bold"))

siniestrosviales_treemap


#grafico evolutivo----
siniestros_viales_grafico_evolutivo <- siniestros_viales_evolutivo %>% 
  ggplot(aes(x = SE_ANIO, y = total_cantidad)) +
  geom_bar(stat = "identity", fill = "#e24a38", width = 0.5) +
  scale_x_discrete(
    breaks = levels(siniestros_viales_evolutivo$SE_ANIO)[seq(1, length(levels(siniestros_viales_evolutivo$SE_ANIO)), by = 3)],
    expand = c(0, 0)) +  
  labs(
    x = "SE-año",
    y = "Lesiones por causas externas") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 25),
    axis.text.x = element_text(size = 25, angle = 90, hjust = 1),
    axis.text.y = element_text(size = 25),
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank())
siniestros_viales_grafico_evolutivo


#grafico edades SE BEM----------
#Armar una tabla para hacer un graf de siniestros viales por grupos etarios SE del BEM
siniestrosviales_tabla_grupoetario <- siniestros_viales %>%
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




siniestrosviales_tabla_grupoetario <- as.data.frame(siniestrosviales_tabla_grupoetario)

#hago la columna GRUPO2 factor para poder ordenarlo

siniestrosviales_tabla_grupoetario$GRUPO_2 <- factor(siniestrosviales_tabla_grupoetario$GRUPO_2, 
                                                     levels = c("< 6 meses","6 a 11 meses",
                                                                "12 a 23 meses","2 a 4 años", "5 a 9 años",
                                                                "10 a 14 años", "15 a 19 años",
                                                                "20 a 24 años", "25 a 34 años", "35 a 44 años",
                                                                "45 a 64 años","65 a 74 años",">= a 75 años",
                                                                "Sin especificar"))

#gráfico siniestros viales por grupo de edad
siniestrosviales_grafico_grupoetario <- siniestrosviales_tabla_grupoetario %>% 
  ggplot(aes(x=Total, y=GRUPO_2))+
  geom_bar(stat = "identity", fill = "#e24a38", width = 0.5)+
  geom_text(aes(label = Total), position = position_dodge(width = 0.5), hjust = - 0.2, size = 2.5) +  # Añadir etiquetas de datos a la barra
  labs(
    x = "Internaciones por siniestros viales",
    y = "Grupos de edad") +
  scale_x_continuous(breaks = seq(0, max(siniestrosviales_tabla_grupoetario$Total), by = 5)) +
  theme_classic () +
  theme(
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6), 
    axis.title.x = element_text(size = 6),  # Cambia el tamaño del título del eje X
    axis.title.y = element_text(size = 6),# Fuente para los textos del eje Y
    panel.border = element_blank(),  # Eliminar borde del panel
    axis.line = element_blank(),  # Eliminar líneas de los ejes
    axis.ticks = element_blank())  # Eliminar "guioncito" de los ejes
siniestrosviales_grafico_grupoetario
