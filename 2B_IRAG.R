#Primero pongo a vrnominal_eventocaso como un dataframe. Recuerden que 
#ggplot y otros paquetes son muy amichis de los dataframe y no tanto de las 
#data.table 


VR_NOMINAL_SINDUPLICADOS<- as.data.frame(VRNOMINAL_EVENTOCASO)

#se bem

SE_BEM_IRAG <- c(23,24,25,26)

#AÑO BEM
AÑO_BEM_IRAG <-  2025


#SEMANA MAXIMA DEL BEM
max_SE_IRAG <- max(SE_BEM_IRAG)


#selecciono hasta la SE de mi BEM

VR_NOMINAL_SINDUPLICADOS <- VR_NOMINAL_SINDUPLICADOS %>%
  filter(
    AÑO < AÑO_BEM_IRAG |
      (AÑO == AÑO_BEM_IRAG & SEPI_ <= max_SE_IRAG))




#armo una tabla con los casos notificados por SE y AÑO
vr_evolutivo <- VR_NOMINAL_SINDUPLICADOS %>% 
  group_by(AÑO, SEPI_,EVENTO)%>% 
  summarise(casos = n(), .groups = "drop") %>% 
  mutate(AÑO_SE = paste(SEPI_, AÑO, sep = "-")) %>% 
  arrange(AÑO, SEPI_) %>% 
  mutate(AÑO_SE = factor(AÑO_SE, levels = unique(AÑO_SE))) %>% 
  tidyr::complete(SEPI_ = 1:max(SEPI_, na.rm = TRUE), fill = list(casos = 0)) %>%
  as.data.frame()



#armo objetos de SE min y año min especifico para VR nominal
VR_min_SE <- vr_evolutivo %>% pull(SEPI_) %>% min(na.rm = TRUE)
VR_min_anio <- vr_evolutivo %>% pull(AÑO) %>% min(na.rm = TRUE)


#objeto total de notificaciones
VR_total_evolutivo <- vr_evolutivo %>% 
  summarise(total_casos = sum(casos, na.rm = TRUE))



#grafico evolutivo
vr_grafico_evolutivo <- vr_evolutivo %>% 
  ggplot(aes(x = AÑO_SE, y = casos, fill = EVENTO)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_x_discrete(
    breaks = levels(vr_evolutivo$AÑO_SE)[seq(1, length(levels(vr_evolutivo$AÑO_SE)), by = 2)],
    expand = c(0, 0)) +
  scale_fill_manual(
    values = c(
      "Internado y/o fallecido por COVID o IRA" = "#4DC86D",
      "Unidad Centinela de Infección Respiratoria Aguda Grave (UC-IRAG)" = "#F2E23C"),
    labels = c(
      "Internado y/o fallecido por COVID o IRA" = "Internado y/o fallecido por COVID o IRA",
      "Unidad Centinela de Infección Respiratoria Aguda Grave (UC-IRAG)" = "UC-IRAG")) +
   scale_y_continuous(
    breaks = pretty(c(0, max(vr_evolutivo$casos, na.rm = TRUE))),
    expand = c(0, 0)) +
  labs(
    x = "SE-año",
    y = "Casos de IRA/IRAG",
    fill = NULL) +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 15, angle = 90, hjust = 1),  # Rotar etiquetas en X
    axis.text.y = element_text(size = 15),  # Fuente para los textos del eje Y
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    strip.text = element_text(size = 15),
    legend.text = element_text(size = 15),       # Tamaño de las etiquetas de la leyenda
    legend.title = element_text(size = 15),        # Tamaño del título de la leyenda
    panel.border = element_blank(),  # Eliminar borde del panel
    axis.line = element_blank(),  # Eliminar líneas de los ejes
    axis.ticks = element_blank(),# Eliminar "guioncito" de los ejes
    legend.position = "top") #cambio el lugar de la leyenda
vr_grafico_evolutivo


#grafico por grupo de edad de COVID/IRA y UC-IRAG

internado_fallec_COVID_IRAG <- VR_NOMINAL_SINDUPLICADOS %>%
  filter(
    AÑO == AÑO_BEM_IRAG,
    SEPI_ %in% SE_BEM_IRAG) %>% 
  group_by(SEPI_, AÑO, GRUPO_ETARIO, EVENTO) %>% 
  summarise(casos = n(), .groups = "drop") %>% 
  mutate(AÑO_SE = paste(SEPI_, AÑO, sep = "-")) %>% 
  arrange(AÑO, SEPI_) %>% 
  mutate(AÑO_SE = factor(AÑO_SE, levels = unique(AÑO_SE))) %>% 
  as.data.frame()


#objeto cantidad SE-BEM
COVID_IRA_UCIRAG <- sum(internado_fallec_COVID_IRAG$casos)



#acomodamos los nombres
internado_fallec_COVID_IRAG <- internado_fallec_COVID_IRAG %>% 
  mutate(
    GRUPO_ETARIO_2 = case_when(
      GRUPO_ETARIO == "Neonato (hasta 28 días)"~"Menor a un año",
      GRUPO_ETARIO == "Neonato (hasta 28 dÍas)"~"Menor a un año",
      GRUPO_ETARIO == "Posneonato (29 hasta 365 dÍas)"~"Menor a un año",
      GRUPO_ETARIO == "De 13 a 24 meses" ~ "13 a 24 meses",
      GRUPO_ETARIO == "De 2 a 4 años"~"2 a 4 años",
      GRUPO_ETARIO == "De 5 a 9 años"~"5 a 9 años",
      GRUPO_ETARIO == "De 10 a 14 años"~"10 a 14 años",
      GRUPO_ETARIO == "De 15 a 19 años"~ "15 a 19 años",
      GRUPO_ETARIO == "De 20 a 24 años"~ "20 a 24 años",
      GRUPO_ETARIO == "De 25 a 34 años"~ "25 a 34 años",
      GRUPO_ETARIO == "De 35 a 44 años"~ "35 a 44 años",
      GRUPO_ETARIO == "De 45 a 65 años"~ "45 a 65 años",
      GRUPO_ETARIO == "Mayores de 65 años"~ "Mayores de 65 años",
      TRUE ~ GRUPO_ETARIO))


orden_edades <- c(
  "Menor a un año",
  "13 a 24 meses",
  "2 a 4 años",
  "5 a 9 años",
  "10 a 14 años",
  "15 a 19 años",
  "20 a 24 años",
  "25 a 34 años",
  "35 a 44 años",
  "45 a 65 años",
  "Mayores de 65 años")

internado_fallec_COVID_IRAG <- internado_fallec_COVID_IRAG %>% 
  mutate(GRUPO_ETARIO_2 = factor(GRUPO_ETARIO_2, levels = orden_edades))


#ambos eventos en la misma barra   

VR_grafico_grupoetario4 <- internado_fallec_COVID_IRAG %>% 
  ggplot(aes(x = casos, y = GRUPO_ETARIO_2, fill = EVENTO)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(
    values = c(
      "Internado y/o fallecido por COVID o IRA" = "#4DC86D",
      "Unidad Centinela de Infección Respiratoria Aguda Grave (UC-IRAG)" = "#F2E23C"),
    labels = c(
      "Internado y/o fallecido por COVID o IRA" = "Internado y/o fallecido por COVID o IRA",
      "Unidad Centinela de Infección Respiratoria Aguda Grave (UC-IRAG)" = "UC-IRAG")) +
  scale_x_continuous(
    breaks = scales::pretty_breaks(),
    labels = scales::label_number(accuracy = 1)) +
    labs(
    x = "Casos notificados",
    y = "Grupos de edad",
    fill = NULL ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    strip.text = element_text(size = 25),
    legend.text = element_text(size = 25),
    legend.title = element_text(size = 25),
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "top")

VR_grafico_grupoetario4


#############################################################################
########################automatizacion de texto #############################
#############################################################################

library(dplyr)
library(lubridate)

# Objeto edades para COVID-IRA
edades_COVID_IRA <- VR_NOMINAL_SINDUPLICADOS %>%
  filter(
    AÑO == AÑO_BEM_IRAG,
    SEPI_ %in% SE_BEM_IRAG,
    ID_SNVS_EVENTO == 330) %>%
  group_by(EDAD_ACTUAL, FECHA_NACIMIENTO, FECHA_APERTURA) %>%
  summarise(casos = n(), .groups = "drop") %>%
  mutate(
    FECHA_NACIMIENTO = as.Date(FECHA_NACIMIENTO),
    FIS = as.Date(FECHA_APERTURA),
    edad_meses = time_length(interval(FECHA_NACIMIENTO, FECHA_APERTURA), "months"),
    edad_calculada_label = case_when(
      edad_meses < 1 ~ "menor a un mes",
      edad_meses < 12 ~ paste0(round(edad_meses), " meses"),
      TRUE ~ paste0(floor(edad_meses / 12), " años")))

# Extraer valores numéricos de edad (en meses)
edades_numericas <- edades_COVID_IRA %>%
  mutate(
    edad_label_limpia = trimws(edad_calculada_label),
    edad_numerica = case_when(
      edad_label_limpia == "menor a un mes" ~ 0,
      grepl("meses", edad_label_limpia) ~ as.numeric(gsub(" meses", "", edad_label_limpia)),
      grepl("años", edad_label_limpia) ~ as.numeric(gsub(" años", "", edad_label_limpia)) * 12,
      TRUE ~ NA_real_))

# Verificar si hay alguna etiqueta con meses o menor a un mes
hay_meses <- any(
  grepl("meses", edades_numericas$edad_calculada_label) |
    edades_numericas$edad_calculada_label == "menor a un mes")

# Edad mínima en texto
edad_min_covid_ira <- if (hay_meses) {
  if (0 %in% edades_numericas$edad_numerica) {
    "menor a un mes"
  } else {
    edad_min_meses <- min(
      edades_numericas$edad_numerica[grepl("meses", edades_numericas$edad_calculada_label)],
      na.rm = TRUE
    )
    if (edad_min_meses == 1) {
      "1 mes"
    } else {
      paste(edad_min_meses, "meses")
    }
  }
} else {
  paste(round(min(edades_numericas$edad_numerica / 12, na.rm = TRUE), 2), "años")
}

# Edad máxima numérica (en meses)
edad_max_meses <- max(edades_numericas$edad_numerica, na.rm = TRUE)

# Convertir edad máxima a texto en meses o años
edad_max_covid_ira <- if (edad_max_meses < 12) {
  if (edad_max_meses == 1) {
    "1 mes"
  } else {
    paste(edad_max_meses, "meses")
  }
} else {
  paste(floor(edad_max_meses / 12), "años")
}

# Promedio de edad (en años, según EDAD_ACTUAL)
edad_promedio_covid_ira <- round(mean(edades_COVID_IRA$EDAD_ACTUAL, na.rm = TRUE))

# Mostrar resultados
edad_min_covid_ira
edad_max_covid_ira
edad_promedio_covid_ira





# objeto edades UC-IRAG----

edades_UC_IRAG <- VR_NOMINAL_SINDUPLICADOS %>%
  filter(
    AÑO == AÑO_BEM_IRAG,
    SEPI_ %in% SE_BEM_IRAG,
    ID_SNVS_EVENTO == 143) %>%
  group_by(EDAD_ACTUAL, FECHA_NACIMIENTO, FECHA_APERTURA) %>%
  summarise(casos = n(), .groups = "drop") %>%
  mutate(
    FECHA_NACIMIENTO = as.Date(FECHA_NACIMIENTO),
    FIS = as.Date(FECHA_APERTURA),
    edad_meses = time_length(interval(FECHA_NACIMIENTO, FECHA_APERTURA), "months"),
    edad_calculada_label = case_when(
      edad_meses < 1 ~ "menor a un mes",
      edad_meses < 12 ~ paste0(round(edad_meses), " meses"),
      TRUE ~ paste0(floor(edad_meses / 12), " años")))

# Convertir edad a valor numérico en meses
edades_numericas_UC_IRAG <- edades_UC_IRAG %>%
  mutate(
    edad_calculada_label = trimws(edad_calculada_label),
    edad_numerica = case_when(
      edad_calculada_label == "menor a un mes" ~ 0,
      grepl("meses", edad_calculada_label) ~ as.numeric(gsub(" meses", "", edad_calculada_label)),
      grepl("años", edad_calculada_label) ~ as.numeric(gsub(" años", "", edad_calculada_label)) * 12
    )
  )

# Verifica si hay registros con edad expresada en meses o menores a 1 mes
hay_meses_UC_IRAG <- any(
  grepl("meses", edades_numericas_UC_IRAG$edad_calculada_label) |
    edades_numericas_UC_IRAG$edad_calculada_label == "menor a un mes"
)

# Calcular edad mínima en formato texto
edad_min_UC_IRAG <- if (hay_meses_UC_IRAG) {
  if (0 %in% edades_numericas_UC_IRAG$edad_numerica) {
    "menor a un mes"
  } else {
    edad_min_meses <- min(
      edades_numericas_UC_IRAG$edad_numerica[grepl("meses", edades_numericas_UC_IRAG$edad_calculada_label)],
      na.rm = TRUE
    )
    if (edad_min_meses == 1) {
      "1 mes"
    } else {
      paste(edad_min_meses, "meses")
    }
  }
} else {
  paste(round(min(edades_numericas_UC_IRAG$edad_numerica / 12, na.rm = TRUE), 2), "años")
}

# Edad máxima (en meses)
edad_max_meses_UC_IRAG <- max(edades_numericas_UC_IRAG$edad_numerica, na.rm = TRUE)

# Convertir edad máxima a texto (meses o años)
edad_max_UC_IRAG <- if (edad_max_meses_UC_IRAG < 12) {
  if (edad_max_meses_UC_IRAG == 1) {
    "1 mes"
  } else {
    paste(edad_max_meses_UC_IRAG, "meses")
  }
} else {
  paste(floor(edad_max_meses_UC_IRAG / 12), "años")
}

# Calcular promedio de edad (usando EDAD_ACTUAL, en años)
edad_promedio_UC_IRAG <- round(mean(edades_UC_IRAG$EDAD_ACTUAL, na.rm = TRUE))

# Mostrar resultados
edad_min_UC_IRAG
edad_max_UC_IRAG
edad_promedio_UC_IRAG


  
  
  
  
  
  
  
  
  