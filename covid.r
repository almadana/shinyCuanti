#Base de datos de covid
library(tidyverse)
#Cargar base de datos ods
library(readODS)
covid <- read_ods("./Toma1_COVID_Alvaro.ods")
# Convertir las variables a factor
covid$SEXO <- factor(covid$SEXO, levels = c(1, 2), labels = c("Hombre", "Mujer"))

covid$NSE_TERCILES <- factor(covid$NSE_TERCILES, levels = c(1, 2, 3), labels = c("Bajo", "Medio", "Alto"))
covid <- covid %>% filter(!is.na(NSE_TERCILES))

covid$TIPO_AISLAMIENTO <- factor(covid$TIPO_AISLAMIENTO,
 levels = c(1, 2, 3, 4, 5), 
 labels = c("Sin aislamiento", "Parcial", "Anterior", "Anterior por razones laborales " , "Total")
)

covid <- covid %>% filter(!is.na(DIAS_AISLAMIENTO))

covid$MIEDO_RETORNO <- factor(covid$MIEDO_RETORNO, levels = c(1, 2, 3), labels = c("Nada", "Poco", "Mucho"))

covid <- covid %>% filter(!is.na(SINTOMAS_ANSIEDAD_ESTADO))

covid$CONSUME_DROGAS <- ifelse(
  covid$DROGAS_AUMENTO_VOLUMEN == "No consume",
  0,
  1
)
covid$CONSUME_DROGAS[is.na(covid$CONSUME_DROGAS)] <- 0
covid$CONSUME_DROGAS <- factor(
  covid$CONSUME_DROGAS,
  levels = c(0, 1),
  labels = c("No consume", "Consume")
)

# Reemplazar NA por "No consume"
covid$DROGAS_AUMENTO_VOLUMEN[is.na(covid$DROGAS_AUMENTO_VOLUMEN)] <- "No consume"
covid$DROGAS_AUMENTO_FRECUENCIA[is.na(covid$DROGAS_AUMENTO_FRECUENCIA)] <- "No consume"

# Reemplazar valores vacíos o diferentes por "No consume"
covid$DROGAS_AUMENTO_VOLUMEN[!(covid$DROGAS_AUMENTO_VOLUMEN %in% c("Si", "No", "No consume"))] <- "No consume"
covid$DROGAS_AUMENTO_FRECUENCIA[!(covid$DROGAS_AUMENTO_FRECUENCIA %in% c("Si", "No", "No consume"))] <- "No consume"

# Convertir a factor con los niveles correctos
covid$DROGAS_AUMENTO_VOLUMEN <- factor(
  covid$DROGAS_AUMENTO_VOLUMEN,
  levels = c("No consume", "No", "Si")
)

covid$DROGAS_AUMENTO_FRECUENCIA <- factor(
  covid$DROGAS_AUMENTO_FRECUENCIA,
  levels = c("No consume", "No", "Si")
)




View(covid)

#Ver las variables de la base de datos
names(covid)
dim(covid)

library(dplyr)
covid <- covid %>% relocate(CONSUME_DROGAS, .after = SINTOMAS_ANSIEDAD_RASGO)

head(covid, 30)
tail(covid, 30)
View(covid)

save(covid, file = "data/covid.RData")