
#================================================================
#------------------- Librerías----------------------------------
#================================================================



library(data.table)    # Para `setDT()` y operaciones rápidas
library(dplyr)         # Para manipulación de datos (`mutate`, `group_by`, etc.)
library(tidyr)         # Para `pivot_longer`
library(stringr)       # Para `str_detect`, `str_subset`, `str_replace`
library(ggplot2)       # Para la visualización
library(readxl)        # para lectura de datos en excel
library(readr)         # para lectura de datos 
library(lubridate)     # para trabajo con fechas


################################################################
#------ACCIÓN-------- > EDITAR PARA CARGAR LA BASE-----------------
################################################################



# Impotación de NQN VR nominal

#Nombre antrerios NOM_P58_VR_[2025-06-09].csv

VR_NOMINAL <- fread("NOM_P58_VR.csv",
  sep = ";",
  encoding = "Latin-1",
  na.strings = c("", "*SIN DATO* (*SIN DATO*)", "*sin dato*", "sin dato", "SIN DATO"))


#  VR_NOMINAL <- read_excel("VR_NOMINAL_Neuquen.xlsx")

#filtro ID prov INDEC residencia = 58
VR_NOMINAL <- VR_NOMINAL %>% 
  filter(ID_PROV_INDEC_RESIDENCIA == 58)


#filtro ID eventos 
#(solo estamos contando "Internado y/o fallecido por COVID o IRA" cuyo ID es 330 y
#"Unidad Centinela de Infección Respiratoria Aguda Grave (UC-IRAG)" cuyo ID es 143)

VR_NOMINAL <- VR_NOMINAL %>% 
  filter(ID_SNVS_EVENTO%in% c(143, 330))



############# SECCIÓN 1:    FUNCIONES  ######################################

#==================================================================
# ------------------ FUNCIÓN PARA FECHAS --------------------------
#==================================================================

# Función para convertir a fecha solo si es necesario
convertir_a_fecha <- function(columna) {
  if (!inherits(columna, "Date")) {  # Verifica si la columna no es de tipo Date
    return(as.Date(columna, format="%d/%m/%Y")) 
  } else {
    return(columna)  # Si ya es Date, la deja como está
  }
}


# Convertir las columnas de IDate a Date
VR_NOMINAL <- VR_NOMINAL %>%
  mutate(
    FECHA_CONSULTA = as.Date(FECHA_CONSULTA),
    FIS = as.Date(FIS),
    FECHA_APERTURA = as.Date(FECHA_APERTURA),
    FECHA_ALTA_MEDICA = as.Date(FECHA_ALTA_MEDICA),
    FECHA_INTERNACION = as.Date(FECHA_INTERNACION),
    FECHA_CUI_INTENSIVOS = as.Date(FECHA_CUI_INTENSIVOS),
    FECHA_FALLECIMIENTO = as.Date(FECHA_FALLECIMIENTO),
    FECHA_NACIMIENTO = as.Date(FECHA_NACIMIENTO),
    FECHA_ESTUDIO = as.Date(FECHA_ESTUDIO)
  ) %>%
  mutate(
    FECHA_ = coalesce(FIS, FECHA_CONSULTA, FECHA_ESTUDIO, FECHA_APERTURA),
    AÑO = year(FECHA_),
    SEPI_ = epiweek(FECHA_)
  )


#================================================================
#------------------- FUNCIÓN COMPLEMENTARIA----------------------
#================================================================


# Cargo las funciones
evaluar_determinaciones <- function(df, fecha_col) {
  # Identificar columnas que comienzan con 'DETERMINACION_'
  cols_determinacion <- grep("^DETERMINACION_", names(df), value = TRUE)
  
  # Crear o resetear la columna 'pendiente_de_revision'
  df[, pendiente_de_revision := 0]
  
  # Vector para almacenar los IDEVENTOCASO afectados antes de la modificación
  ids_algoritmo_aplicado <- c()
  
  # Iterar sobre las columnas de determinaciones
  for (col in cols_determinacion) {
    # Asegurar que la columna no tenga valores NA antes de aplicar grepl
    df[, (col) := as.character(get(col))]
    df[is.na(get(col)), (col) := ""]
    
    # Encontrar celdas con valores que contienen ';'
    condicion <- grepl(";", df[[col]], fixed = TRUE)
    
    # Guardar los IDEVENTOCASO afectados antes de la modificación
    ids_algoritmo_aplicado <- c(ids_algoritmo_aplicado, unique(df[condicion, IDEVENTOCASO]))
    
    # Aplicar condiciones para imputar valores
    df[condicion & !is.na(df[[fecha_col]]), (col) := "Detectable"]
    df[condicion & is.na(df[[fecha_col]]), (col) := paste0(.SD[[col]], ";Pendiente de revision"), .SDcols = col]
    
    # Marcar las filas con 'Pendiente de revision' en la nueva columna
    df[condicion & is.na(df[[fecha_col]]), pendiente_de_revision := 1]
  }
  
  # Guardar los IDEVENTOCASO afectados por la revisión pendiente
  ids_revision <- unique(df[pendiente_de_revision == 1, IDEVENTOCASO])
  
  return(list(df = df, ids_revision = ids_revision, ids_algoritmo_aplicado = unique(ids_algoritmo_aplicado)))
}


#================================================================
#------------------- FUNCIÓN PRINCIPAL---------------------------
#================================================================


# Función principal que ejecuta el algoritmo sobre los datos
algoritmo_1 <- function(data, col_signos, col_comorbilidades, col_determinacion,
                        col_resultado, col_tipo_lugar, col_antecedente, col_cobertura_social) {
  # Convertir el dataset a data.table para mejorar el rendimiento
  setDT(data)
  
  # Identificar los valores únicos en cada columna de comorbilidades y signos/síntomas
  unique_comorbilidades <- unique(na.omit(data[[col_comorbilidades]]))
  unique_signos <- unique(na.omit(data[[col_signos]]))
  
  # Crear columnas dicotómicas para cada comorbilidad
  for (comorbilidad in unique_comorbilidades) {
    nueva_col <- paste0("COMORB_", comorbilidad)
    data[, (nueva_col) := as.integer(get(col_comorbilidades) == comorbilidad)]
  }
  
  # Crear columnas dicotómicas para cada signo/síntoma
  for (signo in unique_signos) {
    nueva_col <- paste0("SINTOMA_", signo)
    data[, (nueva_col) := as.integer(get(col_signos) == signo)]
  }
  
  # Crear columnas para determinación y resultados
  unique_determinaciones <- unique(na.omit(data[[col_determinacion]]))
  
  for (determinacion in unique_determinaciones) {
    nueva_col <- paste0("DETERMINACION_", determinacion)
    data[, (nueva_col) := ifelse(get(col_determinacion) == determinacion, get(col_resultado), NA_character_)]
  }
  
  # Crear columnas para indicar ausencia de valores en determinaciones, signos/síntomas y comorbilidades
  data[, DETERMINACION_SIN_DATO := as.integer(!any(!is.na(.SD[[col_determinacion]]))), by = IDEVENTOCASO]
  data[, SINTOMA_SIN_DATO := as.integer(!any(!is.na(.SD[[col_signos]]))), by = IDEVENTOCASO]
  data[, COMORB_SIN_DATO := as.integer(!any(!is.na(.SD[[col_comorbilidades]]))), by = IDEVENTOCASO]
  
  
  
  # Verificar si 'IDEVENTOCASO' existe para agrupar y consolidar duplicados
  if ("IDEVENTOCASO" %in% names(data)) {
    # Obtener nombres de las columnas dicotómicas y de determinación
    dicotomic_cols <- grep("COMORB_|SINTOMA_", names(data), value = TRUE)
    determinacion_cols <- grep("DETERMINACION_", names(data), value = TRUE)
    
    # Consolidar dicotómicas sumando valores
    data <- data[, c(
      lapply(.SD[, ..dicotomic_cols, with = FALSE], sum, na.rm = TRUE),
      lapply(.SD[, ..determinacion_cols, with = FALSE], function(x) paste(na.omit(unique(x)), collapse = ";")),
      .(TIPO_LUGAR_OCURRENCIA = paste(na.omit(unique(.SD[[col_tipo_lugar]])), collapse = ";")),
      .(ANTECEDENTE_EPIDEMIOLOGICO = paste(na.omit(unique(.SD[[col_antecedente]])), collapse = ";")),
      .(COBERTURA_SOCIAL = paste(na.omit(unique(.SD[[col_cobertura_social]])), collapse = ";")),
      lapply(.SD[, !names(.SD) %in% c(dicotomic_cols, determinacion_cols, col_tipo_lugar, col_antecedente, col_cobertura_social), with = FALSE], function(x) x[1])
    ), by = IDEVENTOCASO]
    
    # Reemplazar valores mayores a 1 por 1 en las columnas dicotómicas
    data[, (dicotomic_cols) := lapply(.SD, function(x) pmin(x, 1, na.rm = TRUE)), .SDcols = dicotomic_cols]
  } else {
    warning("La columna 'IDEVENTOCASO' no está en los datos. Los duplicados no se consolidarán.")
  }
  
  # Aplicar la función evaluar_determinaciones y obtener los IDEVENTOCASO afectados
  resultado_evaluacion <- evaluar_determinaciones(data, "FECHA_ESTUDIO")
  data <- resultado_evaluacion$df
  ids_algoritmo_aplicado <- resultado_evaluacion$ids_algoritmo_aplicado
  ids_revision <- resultado_evaluacion$ids_revision
  
  
  # Guardar el mensaje en un objeto
  mensaje_revision <- paste(
    "Fue aplicado correctamente el algoritmo que transforma el dataset multiregistro para obtener un IDEVENTOCASO por fila (Ver documento xxx),\n",
    "Fueron corregidos (segun algoritmo detallado en la documentación) los ID que presentaban resultados diferentes para una misma determinacion. Estos IDs son:", 
    paste(ids_algoritmo_aplicado, collapse = ", "), "\n",
    "Aquellos IDs que no pudieron corregirse quedaron pendientes de revision:", 
    paste(ids_revision, collapse = ", ")
  )
  
  print(mensaje_revision)
  
  data <- data %>% dplyr::select(-DETERMINACION, -RESULTADO)
  
  return(list(data = data, 
              ids_revision = ids_revision, ids_algoritmo_aplicado = ids_algoritmo_aplicado, mensaje_revision = mensaje_revision
  ))
}

#================================================================
#----------------- FUNCIÓN PARA ANALISIS POR VIRUS---------------
#================================================================

# Función para amnalizar determinaciones
analizar_determinaciones <- function(data, 
                                     columnas_determinacion,
                                     variable_agrupar,
                                     variable_cruce = NULL,
                                     clasificar = NULL) {
  
  data_largo <- data %>%
    pivot_longer(cols = all_of(columnas_determinacion), 
                 names_to = "Tipo_Determinacion", 
                 values_to = "Resultado")
  
  # Agrupar determinaciones usando la función que define la categoría
  data_largo <- data_largo %>%
    mutate(DETERMINACION = if (!is.null(clasificar)) clasificar(Tipo_Determinacion) else Tipo_Determinacion)
  
  
  # Definir agrupadores
  agrupadores <- c(variable_agrupar, if (!is.null(variable_cruce)) variable_cruce, "DETERMINACION")
  
  # Agrupar y contar
  conteo <- data_largo %>%
    group_by(across(all_of(agrupadores))) %>%
    summarise(
      Detectable = sum(str_to_lower(Resultado) %in% c("detectable", "positivo"), na.rm = TRUE),
      No_detectable = sum(str_to_lower(Resultado) %in% c("no detectable", "negativo"), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Total_testeos = Detectable + No_detectable)
  
  return(conteo)
}




################################################################
#-------APLICAR FUNCION   (no modificar nombres)----------------
################################################################


# aplico la fn algoritmo

resultado_algoritmo_1 <- algoritmo_1(data = VR_NOMINAL,
                                     col_signos = "SIGNO_SINTOMA",
                                     col_comorbilidades = "COMORBILIDAD",
                                     col_determinacion= "DETERMINACION", 
                                     col_resultado= "RESULTADO",
                                     col_tipo_lugar =  "TIPO_LUGAR_OCURRENCIA",
                                     col_antecedente = "ANTECEDENTE_EPIDEMIOLOGICO",
                                     col_cobertura_social = "COBERTURA_SOCIAL"
)



# extrae el dataframe procesado
VRNOMINAL_EVENTOCASO <-resultado_algoritmo_1$data

# genera mensaje para comprobar que se realizó la transformación
mensaje5 <- resultado_algoritmo_1$mensaje_revision


# Confirmar que los registros unicos en VR_NOMINAL tienen los mismos registros que VRNOMINAL_EVENTOCASO
n_distinct(VR_NOMINAL$IDEVENTOCASO)



#################  SECCIÓN 2:    ANÁLISIS  ######################################


#======================================================================
#------------- ANÁLISIS POR DETERMINACIÓN -----------------------------
#======================================================================


#---------------------------------------------------------------------
#-------- SE CONSTRUYE DETERMINACIÓN DICOTÓMICA ----------------------
#---------------------------------------------------------------------

#crea una columna que se aplique 1 cuando es positivo
VRNOMINAL_EVENTOCASO<- VRNOMINAL_EVENTOCASO%>%
  mutate(
    DETERMINACION_DICO = case_when(
      DETERMINACION_SIN_DATO == 1 ~ "99",  # Si DETERMINACION_NINGUNA es 1, asignar 99
      rowSums(across(starts_with("DETERMINACION_"), 
                     ~  str_to_lower(.) %in% c("positivo", "detectable"))) > 0 ~ "1",  # Si hay al menos un Positivo o Detectable, asignar 1
      rowSums(across(starts_with("DETERMINACION_"), 
                     ~ str_to_lower(.) %in% c("negativo", "no detectable"))) > 0 ~ "0",  # Si solo hay Negativo o No detectable, asignar 0
      TRUE ~ "99"  # Si no hay información, asignar NA
    )
  )



#---------------------------------------------------------------------
#-------- SE CONSTRUYE DETERMINACIÓN DICOTÓMICA CENTINELA--------------
#---------------------------------------------------------------------


determinacion_UCIRAG <- c(
  "Genoma viral SARS-CoV-2",
  "Genoma viral de Influenza B (sin linaje)",
  "Genoma viral de Influenza A (sin subtipificar)",
  "Genoma viral de VSR",
  "Genoma viral de VSR A",
  "Genoma viral de VSR B",
  "Genoma viral de Influenza A H3N2",
  "Genoma viral de Influenza A H1N1pdm",
  "Genoma viral de Influenza B, linaje Victoria",
  "Genoma viral de Influenza",
  "Genoma viral de Parainfluenza 1",
  "Genoma viral de Parainfluenza 2",
  "Genoma viral de Parainfluenza 3",
  "Genoma viral de Parainfluenza 4")

#grep("Parainfluenza", colnames(VRNOMINAL_EVENTOCASO), value = TRUE)



# Paso 1: Verificamos qué columnas de 'columnas_centinela' existen en el dataframe
columnas_prefijadas <- paste0("DETERMINACION_", determinacion_UCIRAG)
columnas_existentes <- columnas_prefijadas [columnas_prefijadas %in% colnames(VRNOMINAL_EVENTOCASO)]
columnas_faltantes <- setdiff(columnas_prefijadas , columnas_existentes)


# Paso 2: Generamos el mensaje con las columnas faltantes
# Quitar prefijo para mostrar nombres limpios
det_incluidas <- gsub("^DETERMINACION_", "", columnas_existentes)
det_excluidas <- gsub("^DETERMINACION_", "", columnas_faltantes)

# Convertir a listado entre comillas
det_incluidas_listado <- paste0('"', det_incluidas, '"', collapse = "\n")
det_excluidas_listado <- paste0('"', det_excluidas, '"', collapse = ", ")

if (length(columnas_faltantes) > 0) {
  mensaje8 <- paste0(
    "Las siguientes determinaciones incluidas para Estrategia Centinela no existen en el dataframe y fueron omitidas.\n",
    "Es importante revisar que no existan errores de tipeo:\n",
    paste(det_excluidas_listado, collapse = ", "), "\n\n",
    "Determinaciones incluidas utilizadas en el análisis:\n",
    paste(det_incluidas_listado, collapse = ", ")
  )
} else {
  mensaje8 <- paste0(
    "Todas las determinaciones incluidas para Estrategia de vigilancia Centinela existen en la base de datos original.\n\n",
    "Determinaciones incluidas para Estrategia de vigilancia Centinela y utilizadas en este análisis:\n",
    paste(det_incluidas_listado, collapse = ", ")
  )
}


# Paso 3: Aplicamos el mutate solo con las columnas existentes
VRNOMINAL_EVENTOCASO<- VRNOMINAL_EVENTOCASO%>%
  mutate(
    DETERMINACION_DICO_centinela = case_when(
      DETERMINACION_SIN_DATO == 1 ~ "99",
      length(columnas_existentes) > 0 &
        rowSums(across(all_of(columnas_existentes),
                       ~ str_to_lower(.) %in% c("positivo", "detectable"))) > 0 ~ "1",
      length(columnas_existentes) > 0 &
        rowSums(across(all_of(columnas_existentes),
                       ~ str_to_lower(.) %in% c("negativo", "no detectable"))) > 0 ~ "0",
      TRUE ~ "99"))




