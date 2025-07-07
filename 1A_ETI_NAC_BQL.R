#qué hay en este script?#
#Esto es para un gráfico de los 3 eventos juntos según grupo de edad y para texto automatizado


# Cambiar nombres de los grupos de edad
agrupada <- agrupada %>% 
  mutate(
    GRUPO_2 = case_when(
      GRUPO == "< 6 m" ~ "Menor a 6 meses",
      GRUPO == "6 a 11 m" ~ "6 a 11 meses",
      GRUPO == "12 a 23 m" ~ "12 a 23 meses",
      GRUPO == "2 a 4" ~ "2 a 4 años",
      GRUPO == "5 a 9" ~ "5 a 9 años",
      GRUPO == "10 a 14" ~ "10 a 14 años",
      GRUPO == "15 a 19" ~ "15 a 19 años",
      GRUPO == "20 a 24" ~ "20 a 24 años",
      GRUPO == "25 a 34" ~ "25 a 34 años",
      GRUPO == "35 a 44" ~ "35 a 44 años",
      GRUPO == "45 a 64" ~ "45 a 64 años",
      GRUPO == "65 a 74" ~ "65 a 74 años",
      GRUPO == ">= a 75" ~ "75 años y más",
      TRUE ~ GRUPO))

# Filtrar eventos respiratorios
respi_agrupada <- agrupada %>% 
  filter(ID_SNVS_EVENTO_AGRP %in% c(7,443,10,441,8)) %>% 
  filter(ANIO == ANIO_max, SEMANA %in% c(SE_BEM))

# Crear tabla base
grupo_edad_NAC_BQL_ETI <- respi_agrupada %>% 
  group_by(GRUPO_2, ID_SNVS_EVENTO_AGRP, NOMBREEVENTOAGRP) %>%  
  summarize(Total = sum(CANTIDAD, na.rm = TRUE), .groups = "drop")

# Total general
cant_NAC_BQL_ETI <- sum(grupo_edad_NAC_BQL_ETI$Total, na.rm = TRUE)

# Ordenar factor grupos
orden_edades_NAC_BQL_ETI <- c(
  "Menor a 6 meses", "6 a 11 meses", "12 a 23 meses", "2 a 4 años", "5 a 9 años",
  "10 a 14 años", "15 a 19 años", "20 a 24 años", "25 a 34 años", "35 a 44 años",
  "45 a 64 años", "65 a 74 años", "75 años y más")

grupo_edad_NAC_BQL_ETI <- grupo_edad_NAC_BQL_ETI %>%
  mutate(GRUPO_2 = factor(GRUPO_2, levels = orden_edades_NAC_BQL_ETI))

# Cambiar nombres de eventos
grupo_edad_NAC_BQL_ETI <- grupo_edad_NAC_BQL_ETI %>% 
  mutate(NOMBREEVENTOAGRP = case_when(
    str_detect(NOMBREEVENTOAGRP, "influenza") ~ "Enfermedad tipo influenza (ETI)",
    str_detect(NOMBREEVENTOAGRP, "Neumonía") ~ "Neumonía",
    str_detect(NOMBREEVENTOAGRP, "Bronquiolitis") ~ "Bronquiolitis",
    TRUE ~ NOMBREEVENTOAGRP))


grupo_edad_NAC_BQL_ETI <- grupo_edad_NAC_BQL_ETI %>%
  mutate(NOMBREEVENTOAGRP = factor(NOMBREEVENTOAGRP,
                                   levels = c("Neumonía", "Enfermedad tipo influenza (ETI)", "Bronquiolitis")))

# Gráfico
BQL_ETI_NAC_grafico_grupoetario3 <- grupo_edad_NAC_BQL_ETI %>% 
  ggplot(aes(x = GRUPO_2, y = Total, fill = NOMBREEVENTOAGRP)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(x = "Grupos de edad", y = "Casos notificados", fill = "Evento") +
  scale_fill_manual(values = c(
    "Neumonía" = "#2f4858",
    "Enfermedad tipo influenza (ETI)" = "#007463",
    "Bronquiolitis" = "#63dda5" ),
    breaks = c("Enfermedad tipo influenza (ETI)","Bronquiolitis","Neumonía")) +
  coord_flip() +
  theme_classic() +
  theme(
    axis.text = element_text(size = 25),
    axis.title = element_text(size = 25),
    strip.text = element_text(size = 25),
    legend.text = element_text(size = 27),
    legend.title = element_text(size = 27),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "top")


############################################################################
########### objetos para automatizacion de textos ##########################
############################################################################


#uso de slicemax
#La función slice_max() en dplyr se utiliza para seleccionar las filas con los valores máximos de una o más variables dentro de un dataframe o tibble.
help("slice_max") #para estudiar

# --- ETI ----
ETI_gruposedad <- respi_agrupada %>% 
  filter(ID_SNVS_EVENTO_AGRP == 8) %>% 
  group_by(GRUPO_2) %>%  
  summarize(Total = sum(CANTIDAD, na.rm = TRUE), .groups = "drop") %>%  
  mutate(
    Porcentaje_crudo = Total / sum(Total),
    Porcentaje = round(Porcentaje_crudo * 100))

ETI_cantidad <- sum(ETI_gruposedad$Total)

ETI_grupoedad_max <- ETI_gruposedad %>% 
  slice_max(order_by = Total, n = 1, with_ties = FALSE) %>% 
  pull(GRUPO_2) %>% 
  tolower()

ETI_grupoedad_max_cantidad <- ETI_gruposedad %>% 
  slice_max(order_by = Total, n = 1, with_ties = FALSE) %>% 
  pull(Total)

ETI_porcentaje_max <- ETI_gruposedad %>% 
  slice_max(order_by = Porcentaje, n = 1, with_ties = FALSE) %>% 
  pull(Porcentaje)

ETI_grupoedad_min <- ETI_gruposedad %>% 
  slice_min(order_by = Total, n = 1, with_ties = FALSE) %>% 
  pull(GRUPO_2) %>% 
  tolower()

ETI_grupoedad_min_cantidad <- ETI_gruposedad %>% 
  slice_min(order_by = Total, n = 1, with_ties = FALSE) %>% 
  pull(Total)

# --- NAC ----
NAC_gruposedad <- respi_agrupada %>% 
  filter(ID_SNVS_EVENTO_AGRP %in% c(10,441)) %>%  
  mutate(NOMBREEVENTOAGRP2 = "Neumonía") %>%
  group_by(NOMBREEVENTOAGRP2, GRUPO_2) %>% 
  summarize(Total = sum(CANTIDAD, na.rm = TRUE), .groups = "drop") %>%  
  mutate(
    Porcentaje_crudo = Total / sum(Total),
    Porcentaje = round(Porcentaje_crudo * 100))

NAC_cantidad <- sum(NAC_gruposedad$Total)

NAC_grupoedad_max <- NAC_gruposedad %>% 
  slice_max(order_by = Total, n = 1, with_ties = FALSE) %>% 
  pull(GRUPO_2) %>% 
  tolower()

NAC_grupoedad_max_cantidad <- NAC_gruposedad %>% 
  slice_max(order_by = Total, n = 1, with_ties = FALSE) %>% 
  pull(Total)

NAC_porcentaje_max <- NAC_gruposedad %>% 
  slice_max(order_by = Porcentaje, n = 1, with_ties = FALSE) %>% 
  pull(Porcentaje)

NAC_grupoedad_min <- NAC_gruposedad %>% 
  slice_min(order_by = Total, n = 1, with_ties = FALSE) %>% 
  pull(GRUPO_2) %>% 
  tolower()

NAC_grupoedad_min_cantidad <- NAC_gruposedad %>% 
  slice_min(order_by = Total, n = 1, with_ties = FALSE) %>% 
  pull(Total)

# --- BQL ----
BQL_gruposedad <- respi_agrupada %>% 
  filter(ID_SNVS_EVENTO_AGRP %in% c(7,443)) %>%  
  mutate(NOMBREEVENTOAGRP2 = "Bronquiolitis") %>%
  group_by(NOMBREEVENTOAGRP2, GRUPO_2) %>% 
  summarize(Total = sum(CANTIDAD, na.rm = TRUE), .groups = "drop") %>%  
  mutate(
    Porcentaje_crudo = Total / sum(Total),
    Porcentaje = round(Porcentaje_crudo * 100))

BQL_cantidad <- sum(BQL_gruposedad$Total)

BQL_grupoedad_max <- BQL_gruposedad %>% 
  slice_max(order_by = Total, n = 1, with_ties = FALSE) %>% 
  pull(GRUPO_2) %>% 
  tolower()

BQL_grupoedad_max_cantidad <- BQL_gruposedad %>% 
  slice_max(order_by = Total, n = 1, with_ties = FALSE) %>% 
  pull(Total)

BQL_porcentaje_max <- BQL_gruposedad %>% 
  slice_max(order_by = Porcentaje, n = 1, with_ties = FALSE) %>% 
  pull(Porcentaje)

BQL_grupoedad_min <- BQL_gruposedad %>% 
  slice_min(order_by = Total, n = 1, with_ties = FALSE) %>% 
  pull(GRUPO_2) %>% 
  tolower()

BQL_grupoedad_min_cantidad <- BQL_gruposedad %>% 
  slice_min(order_by = Total, n = 1, with_ties = FALSE) %>% 
  pull(Total)










