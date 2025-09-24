# Script auxiliar para generar el diccionario de variables de covid

# Cargar el dataframe covid
load("data/covid.RData")

# Definir las etiquetas manualmente
etiquetas <- c(
  "ID" = "Identificador único",
  "SEXO" = "Sexo del participante",
  "EDAD" = "Edad en años",
  "NSE_TERCILES" = "Nivel socioeconómico",
  "TIPO_AISLAMIENTO" = "Tipo de aislamiento",
  "DIAS_AISLAMIENTO" = "Días de aislamiento en el momento de la encuesta",
  "MIEDO_RETORNO" = "Miedo a retornar a la vida normal",
  "SINTOMAS_DEPRESIVOS" = "Síntomas depresivos. Mayor puntaje = mayor sintomatología",
  "SINTOMAS_ANSIEDAD_ESTADO" = "Estado de ansiedad. Mayor puntaje = mayor sintomatología",
  "SINTOMAS_ANSIEDAD_RASGO" = "Rasgo de ansiedad. Mayor puntaje = mayor sintomatología",
  "CONSUME_DROGAS" = "¿El participante consume drogas?",
  "DROGAS_AUMENTO_FRECUENCIA" = "¿Aumentó la frecuencia de consumo de drogas?", 
  "DROGAS_AUMENTO_VOLUMEN" = "¿Aumentó el volumen de consumo de drogas?"
  # Agrega aquí el resto de variables y sus etiquetas si es necesario
)

# Generar el diccionario

diccionario <- data.frame(
  var = names(covid),
  etiquetas = ifelse(is.na(etiquetas[names(covid)]), "", etiquetas[names(covid)]),
  stringsAsFactors = FALSE
)

# Guardar como CSV
write.csv(diccionario, file = "diccionario_covid.csv", row.names = FALSE) 

# Asignar etiquetas como atributos label a las variables de covid
diccionario <- read.csv("diccionario_covid.csv", stringsAsFactors = FALSE)
for (i in seq_len(nrow(diccionario))) {
  var_name <- diccionario$var[i]
  label <- diccionario$etiquetas[i]
  if (var_name %in% names(covid) && label != "") {
    attr(covid[[var_name]], "label") <- label
  }
}

# Guardar el dataframe con etiquetas
save(covid, file = "data/covid.RData")
# Triada Oscura ----
load("data/darkTriad.RData")
etiquetas <- c(
  "Edad" = "Edad en años",
  "Sexo" = "Sexo del participante",
  "Lugar_procedencia" = "Lugar de procedencia",
  "Resi_actual" = "Lugar de residencia actual",
  "Veces_entra_FBIG_XDIA" = "Número de veces que entra en Facebook o Instagram al día",
  "Sigue_a" = "Cuántas personas sigue en Instagram",
  "Le_siguen" = "Cuántos seguidores tiene en Instagram",
  "Refleja2" = "Plataforma que utiliza más (Facebook o Instagram)",
  "Maquiavelismo_SD3" = "Puntaje de Maquiavelismo (SD3)",
  "Narcicismo_SD3" = "Puntaje de Narcisismo (SD3)",
  "Psicopatia_SD3" = "Puntaje de Psicopatía (SD3)",
  "Satisfaccion_con_la_vida" = "Satisfacción con la vida"
)
 # ... existing code ...
# ---- Triada Oscura: generar y aplicar diccionario ----
# Vector de etiquetas (puedes completar las que correspondan a cada variable de dt)
# Si ya definiste 'etiquetas' arriba, se usará; si definiste 'etiquetas_tri', también se respetará
if (exists("etiquetas_tri") && length(etiquetas_tri) > 0) {
  etq_src <- etiquetas_tri
} else if (exists("etiquetas") && length(etiquetas) > 0) {
  etq_src <- etiquetas
} else {
  etq_src <- c()
}

# Generar diccionario para Triada Oscura

dic_tri <- data.frame(
  var = names(dt),
  etiquetas = ifelse(is.na(etq_src[names(dt)]), "", as.character(etq_src[names(dt)])),
  stringsAsFactors = FALSE
)

# Guardar diccionario como CSV
write.csv(dic_tri, file = "./data/diccionario_darkTriad.csv", row.names = FALSE)

# Asignar etiquetas como atributo label cuando existan
for (i in seq_len(nrow(dic_tri))) {
  var_name <- dic_tri$var[i]
  label <- dic_tri$etiquetas[i]
  if (var_name %in% names(dt) && !is.na(label) && label != "") {
    attr(dt[[var_name]], "label") <- label
  }
}

# Guardar el objeto con etiquetas
save(dt, file = "./data/darkTriad.RData")

#Censo Recodificado ----
load("data/censo_recod.RData")
etiquetas <- c(
  "Sexo" = "Sexo del participante",
  "Edad" = "Edad en años",
  "Lugar_Nac" = "Lugar de nacimiento",
  "Dep_Nac" = "Departamento de nacimiento",
  "País" = "País",
  "Nro_trabajos" = "Número de trabajos que tiene",
  "Estado_conyugal" = "Estado conyugal",
  "Num_hijos" = "Número de hijos",
  "Posición_Ocupacional" = "Tipo de trabajo que tiene",
  "Único_percibe_ingresos_hogar" = "¿Único percibe ingresos en el hogar?",
  "Vivienda_Ud_es" = "Condición de tenencia de vivienda",
  "ocupacion_grupo" = "Grupo de ocupación según OIT",
  "Área_inserción" = "Área de inserción agrupada"
)

# Generar diccionario para Censo Recodificado
dic_censo_recod <- data.frame(
  var = names(censoRec),
  etiquetas = ifelse(is.na(etiquetas[names(censoRec)]), "", 
                     as.character(etiquetas[names(censoRec)])),
  stringsAsFactors = FALSE
)

# Asignar etiquetas como atributo label cuando existan
for (i in seq_len(nrow(dic_censo_recod))) {
  var_name <- dic_censo_recod$var[i]
  label <- dic_censo_recod$etiquetas[i]
  if (var_name %in% names(censoRec) && !is.na(label) && label != "") {
    attr(censoRec[[var_name]], "label") <- label
  }
}

# Guardar el objeto con etiquetas
save(censoRec, file = "./data/censo_recod.RData")
write.csv(dic_censo_recod, file = "./data/diccionario_censo_recod.csv", 
          row.names = FALSE)
