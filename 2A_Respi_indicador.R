# no voy a cargar bases que ya cargue, voy a hacer source con esos scripts
source("1_agrupada.R")
source("2_VRNOMINAL.R")
source("2B_IRAG.R")
source("1A_ETI_NAC_BQL.R")


#Tabla de IRAG -----
vr_evolutivo_indicador <- as.data.frame(vr_evolutivo)
 
#agrego el filtro de las SE del BEM y año del BEM
vr_evolutivo_indicador2 <- vr_evolutivo_indicador %>%
  filter(AÑO == AÑO_BEM_IRAG & SEPI_ %in% SE_BEM_IRAG)

#calculo el N de las IRAG-----

cant_IRAG <- vr_evolutivo_indicador2 %>% 
  summarize(total_casos = sum(casos, na.rm = TRUE))


#tabla ETI----
ETI_indicador <- respi_agrupada %>% 
  filter(ID_SNVS_EVENTO_AGRP == 8) %>% 
  group_by(SEMANA, ANIO, NOMBREEVENTOAGRP) %>%  
  summarize(Total = sum(CANTIDAD, na.rm = TRUE), .groups = "drop")

#calculo de N de las ETI
cant_ETI <- ETI_indicador %>% 
  summarize(Total = sum(Total, na.rm = TRUE))


#tabla NAC----

NAC_indicador <- respi_agrupada %>% 
  filter(ID_SNVS_EVENTO_AGRP %in% c(10,441))%>% 
  group_by(SEMANA, ANIO, NOMBREEVENTOAGRP) %>%  
  summarize(Total = sum(CANTIDAD, na.rm = TRUE), .groups = "drop")

#calculo de N de las NAC
cant_NAC <- NAC_indicador %>% 
  summarize(Total = sum(Total, na.rm = TRUE))


#calculo de N de las BQL----
BQL_indicadores <- respi_agrupada %>% 
  filter(ID_SNVS_EVENTO_AGRP %in% c(7,443)) %>% 
  group_by(SEMANA, ANIO, NOMBREEVENTOAGRP) %>%  
  summarize(Total = sum(CANTIDAD, na.rm = TRUE), .groups = "drop")

#calculo de N de las BQL
cant_BQL <- BQL_indicadores %>% 
  summarize(Total = sum(Total, na.rm = TRUE))

#armo el total de las IRAG, ETI, NAC y BQL de las SE y año BEM----
total_indicador <- sum(cant_BQL,cant_ETI,cant_NAC,cant_IRAG)


##########################################################################
######### esto es del año anterior para calcular variacion % #############
##########################################################################
 
#IRAG año anterior------
#tabla
vr_anio_anterior <- vr_evolutivo_indicador %>% 
  filter(AÑO == AÑO_BEM_IRAG-1 & SEPI_ %in% SE_BEM_IRAG)

#calculo el N de las IRAG año anterior

cant_IRAG_año_anterior <- vr_anio_anterior %>% 
  summarize(total_casos = sum(casos, na.rm = TRUE))

#ETI año anterior----

ETI_año_anterior <- agrupada %>% 
  filter(ID_SNVS_EVENTO_AGRP == 8) %>% 
  filter(ANIO == AÑO_BEM_IRAG-1 & SEMANA %in% SE_BEM_IRAG) %>% 
  group_by(SEMANA, ANIO) %>%  
  summarize(Total = sum(CANTIDAD, na.rm = TRUE), .groups = "drop")

#calculo el N de las ETI año anterior
cant_ETI_año_anterior <- ETI_año_anterior %>% 
  summarize(Total = sum(Total, na.rm = TRUE))


#NAC año anterior----

NAC_año_anterior <- agrupada %>% 
  filter(ID_SNVS_EVENTO_AGRP %in% c(10,441))%>% 
  filter(ANIO == AÑO_BEM_IRAG-1 & SEMANA %in% SE_BEM_IRAG) %>% 
  group_by(SEMANA, ANIO) %>%  
  summarize(Total = sum(CANTIDAD, na.rm = TRUE), .groups = "drop")


#calculo de N de las NAC
cant_NAC_año_anterior <- NAC_año_anterior %>% 
  summarize(Total = sum(Total, na.rm = TRUE))


#BQL año anterior----
BQL_año_anterior <- respi_agrupada %>% 
  filter(ID_SNVS_EVENTO_AGRP %in% c(7,443)) %>% 
  filter(ANIO == AÑO_BEM_IRAG-1 & SEMANA %in% SE_BEM_IRAG) %>% 
  group_by(SEMANA, ANIO) %>%  
  summarize(Total = sum(CANTIDAD, na.rm = TRUE), .groups = "drop")

#calculo de N de las BQL
cant_BQL_año_anterior <- BQL_año_anterior %>% 
  summarize(Total = sum(Total, na.rm = TRUE))


#N IRAG, BQL, ETI y NAC año anterior 
total_indicador_año_anterior <- sum(cant_BQL_año_anterior,cant_ETI_año_anterior,cant_NAC_año_anterior,cant_IRAG_año_anterior)


#########################################################################
##########CALCULO DE VARIACION PORCENTUAL################################
#########################################################################

RESPI_variacion_porcentual <- 
  round((total_indicador - total_indicador_año_anterior)/total_indicador_año_anterior*100,1)


#########################################################################
#########################################################################
###############SET INDICADORES- recuadro propiamente dicho###############
#########################################################################
#########################################################################


# Convertir el valor numérico de la variación a texto con el signo %
RESPI_variacion_porcentual_texto <- paste0(RESPI_variacion_porcentual, "%")


#creo objeto de aumento o disminución
IRA_texto_variacion <- ifelse(RESPI_variacion_porcentual < 0, "una disminución", "un aumento")



#  Crear el diseño del recuadro
indicador_IRA <- ggplot() +
  # Fondo superior (azul oscuro)
  annotate("rect", xmin = 0, xmax = 1, ymin = 0.66, ymax = 1, fill = "#289132", alpha = 0.9) +
  # Fondo central (blanco)
  annotate("rect", xmin = 0, xmax = 1, ymin = 0.33, ymax = 0.66, fill = "#faf7ff", alpha = 0.8) +
  # Fondo inferior (azul más claro)
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 0.33, fill = "#e6efff", alpha = 0.9) +
  # Título en la parte superior
  annotate("text", x = 0.5, y = 0.83, label = "Infecciones respiratorias agudas", size = 10, fontface = "bold", color = "white") +
  # Número grande en el centro
  annotate("text", x = 0.5, y = 0.495, label = total_indicador, size = 10, fontface = "bold", color = "#289132") +
  # Variación porcentual en la banda inferior
  annotate("text", x = 0.1, y = 0.165, label = "Variación", size = 8, fontface = "bold", color = "#289132", hjust = 0) +
  annotate("text", x = 0.9, y = 0.165, label = RESPI_variacion_porcentual_texto, size = 8, fontface = "bold", color = "#289132", hjust = 1) +
  theme_void()

indicador_IRA



##############################################################################
#####################GRAFICO TREEMAP##########################################
##############################################################################

IRA_juntas <- bind_rows(ETI_indicador,BQL_indicadores,NAC_indicador)

#chequeo los nombres de las tablas
names(vr_evolutivo_indicador2)
names(IRA_juntas)
names(BQL_indicadores)
names(NAC_indicador)


#cambio nombres
IRA_juntas <- IRA_juntas %>%
  rename(Evento = NOMBREEVENTOAGRP)

#cambio nombre a las IRAG para poder juntarlas con IRA_
vr_evolutivo_indicador_nombrescambiados <- vr_evolutivo_indicador2 %>% 
  rename(SEMANA = SEPI_,
         ANIO = AÑO,
         Evento = EVENTO,
         Total = casos) %>% 
  select(-AÑO_SE)

#Treemap----


#junto las tablas
IRA_juntas <- bind_rows(IRA_juntas,vr_evolutivo_indicador_nombrescambiados)


tabla_IRAG_BQL_NAC_ETI <- tibble::tibble(
  evento = c("IRAG", "Bronquiolitis", "Neumonía", "Enfermedad tipo influenza (ETI)"),
  cantidad = c(cant_IRAG, cant_BQL, cant_NAC, cant_ETI)) %>% 
  mutate(cantidad = as.numeric(cantidad), 
         porcentaje = round(cantidad / sum(cantidad) * 100))


#grafico

IRA_treemap <- tabla_IRAG_BQL_NAC_ETI %>% 
  ggplot(aes(area = cantidad, fill = cantidad, 
             label = paste0(evento, "\n", cantidad))) +  
  geom_treemap() +
  geom_treemap_text(colour = "white", 
                    place = "center",
                    size = 12,
                    reflow = TRUE,
                    grow = FALSE) +  
  scale_fill_gradient(low = "#4eb250", high = "#289132") +  
  theme(legend.position = "none",
        text = element_text(family = "Montserrat"),  
        plot.title = element_text(face = "bold"))

IRA_treemap


##objetos para la descripción

tabla_IRAG_BQL_NAC_ETI2 <- tabla_IRAG_BQL_NAC_ETI %>%
  arrange(desc(cantidad))

#cambio los nombres 
tabla_IRAG_BQL_NAC_ETI2 <- tabla_IRAG_BQL_NAC_ETI2 %>% 
  mutate(evento = case_when(
    evento == "IRAG" ~ "IRAG",
    evento == "Bronquiolitis" ~ "bronquiolitis", 
    evento == "Neumonía" ~ "neumonías",
    evento == "Enfermedad tipo influenza (ETI)" ~ "ETI",
    TRUE ~ evento))



#objeto texto % de notificaciones
evento1 <- tabla_IRAG_BQL_NAC_ETI2$evento[1]

porcentaje1 <- tabla_IRAG_BQL_NAC_ETI2$porcentaje[1]

##
evento2 <- tabla_IRAG_BQL_NAC_ETI2$evento[2]

porcentaje2 <- tabla_IRAG_BQL_NAC_ETI2$porcentaje[2]

##
evento3 <- tabla_IRAG_BQL_NAC_ETI2$evento[3]
porcentaje3 <- tabla_IRAG_BQL_NAC_ETI2$porcentaje[3]

##
evento4 <- tabla_IRAG_BQL_NAC_ETI2$evento[4]
porcentaje4 <- tabla_IRAG_BQL_NAC_ETI2$porcentaje[4]



