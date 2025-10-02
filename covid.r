
# Cargar base de datos
covid <- read_ods("./Toma1_COVID_Alvaro.ods")

# Convertir variables a factores con etiquetas
covid$SEXO <- factor(covid$SEXO, levels = c(1, 2), labels = c("Hombre", "Mujer"))
covid$NSE_TERCILES <- factor(covid$NSE_TERCILES, levels = c(1, 2, 3), labels = c("Bajo", "Medio", "Alto"))
covid <- covid %>% filter(!is.na(NSE_TERCILES))

covid$TIPO_AISLAMIENTO <- factor(
  covid$TIPO_AISLAMIENTO,
  levels = c(1, 2, 3, 4, 5),
  labels = c("Sin aislamiento", "Parcial", "Anterior", "Anterior por razones laborales", "Total")
)
covid <- covid %>% filter(!is.na(DIAS_AISLAMIENTO))

covid$MIEDO_RETORNO <- factor(covid$MIEDO_RETORNO, levels = c(1, 2, 3), labels = c("Nada", "Poco", "Mucho"))
covid <- covid %>% filter(!is.na(SINTOMAS_ANSIEDAD_ESTADO))

# Re-codificación de DROGAS_AUMENTO_VOLUMEN
covid$DROGAS_AUMENTO_VOLUMEN <- case_when(
  is.na(covid$DROGAS_AUMENTO_VOLUMEN) ~ "No consume",
  covid$DROGAS_AUMENTO_VOLUMEN == 1 ~ "No",
  covid$DROGAS_AUMENTO_VOLUMEN == 2 ~ "Sí",
  TRUE ~ "Otro"
)

covid$DROGAS_AUMENTO_VOLUMEN <- factor(
  covid$DROGAS_AUMENTO_VOLUMEN,
  levels = c("No consume", "No", "Sí")
)

# Re-codificación de DROGAS_AUMENTO_FRECUENCIA
covid$DROGAS_AUMENTO_FRECUENCIA <- case_when(
  is.na(covid$DROGAS_AUMENTO_FRECUENCIA) ~ "No consume",
  covid$DROGAS_AUMENTO_FRECUENCIA == 1 ~ "No",
  covid$DROGAS_AUMENTO_FRECUENCIA == 2 ~ "Sí",
  TRUE ~ "Otro"
)

covid$DROGAS_AUMENTO_FRECUENCIA <- factor(
  covid$DROGAS_AUMENTO_FRECUENCIA,
  levels = c("No consume", "No", "Sí")
)

# Crear CONSUME_DROGAS en base a DROGAS_AUMENTO_VOLUMEN
covid$CONSUME_DROGAS <- case_when(
  is.na(covid$DROGAS_AUMENTO_VOLUMEN) ~ "No consume",
  covid$DROGAS_AUMENTO_VOLUMEN %in% c("No", "Sí") ~ "Consume",
  TRUE ~ "No consume"
)

covid$CONSUME_DROGAS <- factor(covid$CONSUME_DROGAS, levels = c("No consume", "Consume"))

# Reubicar la variable
covid <- covid %>% relocate(CONSUME_DROGAS, .after = SINTOMAS_ANSIEDAD_RASGO)

# Guardar base
save(covid, file = "data/covid.RData")

#Generar un diccionario de las variables
etiquetas <- c(
  "ID" = "Identificador único",
  "SEXO" = "Sexo del participante",
  "EDAD" = "Edad en años",
  "NSE_TERCILES" = "Nivel socioeconómico",
  "TIPO_AISLAMIENTO" = "Tipo de aislamiento",
  "DIAS_AISLAMIENTO" = "Días de aislamiento en el momento de la encuesta",
  "MIEDO_RETORNO" = "Miedo a retornar a la vida normal",
  "SINTOMAS_DEPRESIVOS" = "Síntomas depresivos: medidos con el BDI-II (Beck Depression Inventory II)",
  "SINTOMAS_ANSIEDAD_ESTADO" = "Estado de ansiedad, medido con el STAI (State-Trait Anxiety Inventory)",
  "SINTOMAS_ANSIEDAD_RASGO" = "Rasgo de ansiedad, medido con el STAI (State-Trait Anxiety Inventory).",
  "CONSUME_DROGAS" = "¿El participante consume drogas? (tanto de uso recreativo como psicofármacos)",
  "DROGAS_AUMENTO_FRECUENCIA" = "¿Aumentó la frecuencia de consumo de drogas?", 
  "DROGAS_AUMENTO_VOLUMEN" = "¿Aumentó el volumen de consumo de drogas?"  # ... agrega el resto de variables y sus etiquetas aquí ...
)

diccionario <- data.frame(
  var = names(covid),
  label = etiquetas[names(covid)],
  stringsAsFactors = FALSE
)

# Si alguna variable no tiene etiqueta, quedará como NA. Puedes reemplazar NA por "" si prefieres:
diccionario$etiquetas[is.na(diccionario$etiquetas)] <- ""

write.csv(diccionario, file = "diccionario_covid.csv", row.names = FALSE)

covid = add_labels(covid,diccionario)
save(covid,file="data/covid.RData")
