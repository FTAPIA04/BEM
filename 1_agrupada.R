#objetivos:
#a)cargar dataset
#b)crear objetos SE BEM, AÑO MAX, AÑO MIN
#c)agregar columnas regiones 


#aclaración: la base agrupada viene solo con los años 2024 y 2025.Se pidio
#a nación una base completa. Lo primero que se hace acá es unir las 2 bases


#cargo la base agrupada completa, sin años 2024 y 2025

agrupada_vieja <- read.csv("NEUQUEN_CLI_ANTERIOR.csv", sep = ";",na.strings = "")
sum(complete.cases(agrupada_vieja))

agrupada_vieja <- agrupada_vieja %>%
  filter(ANIO < 2024 )

###################################################################
##############cargo base agrupada actual###########################
###################################################################
  
##cargo base agrupada actual###
#Nombre anterior CLI_P58_[2025-06-09].csv

  agrupada_2025 <- fread("CLI_P58.csv", sep = ";",
                    encoding = "Latin-1",
                    na.strings = c("", "*SIN DATO* (*SIN DATO*)", "*sin dato*", "sin dato", "SIN DATO"))
  sum(complete.cases(agrupada_2025))
  
  nrow(agrupada_2025) #es un chequeo 
  
  
  
  
##########################################################################
######### junto base agrupada vieja con actual ###########################
##########################################################################
#primero fuerzo a que las columnas de ambas bases (agrupada y agrupada vieja) 
  #tengan el mismo formato. Sin esto, no se puede usar bind_rowns
  agrupada_vieja <- agrupada_vieja |> mutate(across(everything(), as.character))
  agrupada_2025 <- agrupada_2025 |> mutate(across(everything(), as.character))
  
 
  
help("bind_rows") 

  #Esto no es un cruce, sino que pongo una arriba de la otra. Distinto a join/left join/full join, etc.
   agrupada_2425 <- bind_rows(agrupada_vieja, agrupada_2025)
  
#hasta acá hay SE 2025 
   
   
######################################################################
###### agrego columna region #########################################
######################################################################
  
#cargo base de regiones 
  regiones <- read_excel("REGIONES.xlsx")
  regiones <- as.data.frame(regiones) #cambio formato a dataframe
  
#ID_origen con formato character ambas, para poder cruzar
  agrupada_2425 <- agrupada_2425 |> mutate(ID_ORIGEN = as.character(ID_ORIGEN))
  regiones <- regiones |> mutate(ID_ORIGEN = as.character(ID_ORIGEN))
    
  #agrego la columna regiones e ID regiones a agrupadas
  agrupada_regiones <- agrupada_2425 %>%
    left_join(regiones, by = "ID_ORIGEN")  
  
  
 #hasta acá hay SE 2025 
  
  help("left_join") #para estudiar 
    
########################################################################
################### Seteo de base agrupada #############################
########################################################################
  
##SE/año del BEM
#las SE/año del BEM de este mes son   (esto es un ayuda memoria) del año  #SE maxima
  SE_BEM <- c(23, 24, 25, 26) #hay que cambiarlo mensualmente
  SE_BEM <- as.vector(SE_BEM) 
  
#armo un objeto numerico con mi semana máxima 
  SE_BEM_MAX <- max(SE_BEM)
     
#selecciono hasta la SE de mi BEM. Estas líneas filtra:
  #que "ANIO" sea menor a 2025 (serían todas las SE de los años menores a 2025), 
  #que de 2025 filtre hasta la SE máxima de la actualización del BEM
  agrupada <- agrupada_regiones %>%
    mutate(
      ANIO = as.integer(ANIO),
      SEMANA = as.integer(SEMANA)) %>%
    filter(
      ANIO < 2025 | (ANIO == 2025 & SEMANA <= SE_BEM_MAX))
  
  
  
  
 

  
  
  
#############################################################################
############# creación de SE-año, año máx/min, año anterior##################
#############################################################################

#columna se-año, SE MAX, MIN----
#armo la variable SE-año
  agrupada <- agrupada %>%
    mutate(SE_ANIO= paste(SEMANA, ANIO, sep = "-"))
  
 
  
  #ANIO maximo (lo voy a usar para tablas, gráficos)
  ANIO_max <- agrupada %>% 
    summarise(ANIO_max = max(ANIO, na.rm = TRUE))
  
  ANIO_max <- as.numeric(ANIO_max[[1]])  # Extrae la primera columna y lo convierte a numérico
  
  #SE_min 
  SE_min <- SE_BEM %>%
    min()
  
  #SE_max 
  SE_max <- SE_BEM %>%
    max()
  
  #AÑO ANTERIOR AL ACTUAL PARA VARIACION %
  anio_anterior <- ANIO_max-1
    

 agrupada <- agrupada %>%
    mutate(CANTIDAD = as.numeric(CANTIDAD))

agrupada <- agrupada %>% 
  mutate(SEMANA = as.numeric(SEMANA))

agrupada <- agrupada %>% 
  mutate(ANIO=as.numeric(ANIO))

