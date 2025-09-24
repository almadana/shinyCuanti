# ANÁLISIS SUGERIDOS PARA DATASET COVID-19
# ===============================================
# Dataset: Impacto psicológico del COVID-19
# N = 1,030 participantes
# ===============================================

library(tidyverse)
library(ggplot2)
library(corrplot)
library(gridExtra)
library(car)
library(psych)
library(GGally)
library(plotly)

# Cargar datos
load("data/covid.RData")

# ===============================================
# 1. ANÁLISIS EXPLORATORIO DE DATOS (EDA)
# ===============================================

# --- 1.1 CARACTERÍSTICAS DEMOGRÁFICAS ---
cat("=== ANÁLISIS DEMOGRÁFICO ===\n")

# Distribución por sexo y edad
p1 <- ggplot(covid, aes(x = SEXO, fill = SEXO)) +
  geom_bar(alpha = 0.7) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(title = "Distribución por Sexo", y = "Frecuencia") +
  theme_minimal() +
  scale_fill_manual(values = c("#3498db", "#e74c3c"))

p2 <- ggplot(covid, aes(x = EDAD)) +
  geom_histogram(bins = 20, fill = "#2ecc71", alpha = 0.7, color = "white") +
  geom_vline(aes(xintercept = mean(EDAD)), color = "red", linetype = "dashed", size = 1) +
  labs(title = "Distribución de Edad", x = "Edad", y = "Frecuencia") +
  theme_minimal()

p3 <- ggplot(covid, aes(x = NSE_TERCILES, fill = NSE_TERCILES)) +
  geom_bar(alpha = 0.7) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(title = "Nivel Socioeconómico", y = "Frecuencia") +
  theme_minimal() +
  scale_fill_manual(values = c("#e67e22", "#f39c12", "#27ae60"))

grid.arrange(p1, p2, p3, ncol = 2)

# --- 1.2 ANÁLISIS DEL AISLAMIENTO ---
cat("\n=== ANÁLISIS DEL AISLAMIENTO ===\n")

p4 <- ggplot(covid, aes(x = TIPO_AISLAMIENTO, fill = TIPO_AISLAMIENTO)) +
  geom_bar(alpha = 0.7) +
  coord_flip() +
  geom_text(stat = "count", aes(label = ..count..), hjust = -0.1) +
  labs(title = "Tipos de Aislamiento", x = "", y = "Frecuencia") +
  theme_minimal() +
  theme(legend.position = "none")

p5 <- ggplot(covid, aes(x = DIAS_AISLAMIENTO)) +
  geom_histogram(bins = 15, fill = "#9b59b6", alpha = 0.7, color = "white") +
  geom_vline(aes(xintercept = mean(DIAS_AISLAMIENTO)), color = "red", linetype = "dashed") +
  labs(title = "Distribución Días de Aislamiento", x = "Días", y = "Frecuencia") +
  theme_minimal()

grid.arrange(p4, p5, ncol = 2)

# ===============================================
# 2. ANÁLISIS DE SALUD MENTAL
# ===============================================

cat("\n=== ANÁLISIS DE SALUD MENTAL ===\n")

# Crear dataset de variables de salud mental
mental_health <- covid %>%
  select(SINTOMAS_DEPRESIVOS, SINTOMAS_ANSIEDAD_ESTADO, SINTOMAS_ANSIEDAD_RASGO, MIEDO_RETORNO)

# Matriz de correlaciones
cor_matrix <- cor(mental_health[,1:3])
corrplot(cor_matrix, method = "color", type = "upper", 
         order = "hclust", tl.col = "black", tl.srt = 45,
         title = "Correlaciones entre Variables de Salud Mental")

# Distribuciones de síntomas
p6 <- ggplot(covid, aes(x = SINTOMAS_DEPRESIVOS)) +
  geom_histogram(bins = 20, fill = "#34495e", alpha = 0.7, color = "white") +
  labs(title = "Síntomas Depresivos", x = "Puntaje", y = "Frecuencia") +
  theme_minimal()

p7 <- ggplot(covid, aes(x = SINTOMAS_ANSIEDAD_ESTADO)) +
  geom_histogram(bins = 20, fill = "#e74c3c", alpha = 0.7, color = "white") +
  labs(title = "Ansiedad Estado", x = "Puntaje", y = "Frecuencia") +
  theme_minimal()

p8 <- ggplot(covid, aes(x = SINTOMAS_ANSIEDAD_RASGO)) +
  geom_histogram(bins = 20, fill = "#f39c12", alpha = 0.7, color = "white") +
  labs(title = "Ansiedad Rasgo", x = "Puntaje", y = "Frecuencia") +
  theme_minimal()

p9 <- ggplot(covid, aes(x = MIEDO_RETORNO, fill = MIEDO_RETORNO)) +
  geom_bar(alpha = 0.7) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(title = "Miedo al Retorno", x = "", y = "Frecuencia") +
  theme_minimal() +
  scale_fill_manual(values = c("#27ae60", "#f39c12", "#e74c3c"))

grid.arrange(p6, p7, p8, p9, ncol = 2)

# ===============================================
# 3. ANÁLISIS DE CONSUMO DE DROGAS
# ===============================================

cat("\n=== ANÁLISIS CONSUMO DE DROGAS ===\n")

# Tabla cruzada consumo vs aumento
tabla_drogas <- table(covid$CONSUME_DROGAS, covid$DROGAS_AUMENTO_FRECUENCIA)
print("Tabla cruzada: Consumo vs Aumento de Frecuencia")
print(tabla_drogas)

# Visualización
p10 <- ggplot(covid, aes(x = CONSUME_DROGAS, fill = DROGAS_AUMENTO_FRECUENCIA)) +
  geom_bar(position = "dodge", alpha = 0.8) +
  labs(title = "Consumo y Aumento de Frecuencia de Drogas", 
       x = "Consume Drogas", y = "Frecuencia", fill = "Aumentó Frecuencia") +
  theme_minimal()

print(p10)

# ===============================================
# 4. ANÁLISIS COMPARATIVOS POR GRUPOS
# ===============================================

cat("\n=== ANÁLISIS COMPARATIVOS ===\n")

# Síntomas por sexo
p11 <- ggplot(covid, aes(x = SEXO, y = SINTOMAS_DEPRESIVOS, fill = SEXO)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  labs(title = "Síntomas Depresivos por Sexo", y = "Puntaje Depresión") +
  theme_minimal() +
  scale_fill_manual(values = c("#3498db", "#e74c3c"))

# Ansiedad por tipo de aislamiento
p12 <- ggplot(covid, aes(x = TIPO_AISLAMIENTO, y = SINTOMAS_ANSIEDAD_ESTADO, fill = TIPO_AISLAMIENTO)) +
  geom_boxplot(alpha = 0.7) +
  coord_flip() +
  labs(title = "Ansiedad Estado por Tipo de Aislamiento", 
       x = "", y = "Puntaje Ansiedad") +
  theme_minimal() +
  theme(legend.position = "none")

grid.arrange(p11, p12, ncol = 1)

# Síntomas por NSE
p13 <- ggplot(covid, aes(x = NSE_TERCILES, y = SINTOMAS_DEPRESIVOS, fill = NSE_TERCILES)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Síntomas Depresivos por Nivel Socioeconómico", 
       x = "NSE", y = "Puntaje Depresión") +
  theme_minimal() +
  scale_fill_manual(values = c("#e67e22", "#f39c12", "#27ae60"))

print(p13)

# ===============================================
# 5. ANÁLISIS ESTADÍSTICOS INFERENCIALES
# ===============================================

cat("\n=== PRUEBAS ESTADÍSTICAS ===\n")

# Test t para diferencias por sexo en depresión
t_test_sexo <- t.test(SINTOMAS_DEPRESIVOS ~ SEXO, data = covid)
cat("Test t - Síntomas depresivos por sexo:\n")
print(t_test_sexo)

# ANOVA para diferencias por NSE en ansiedad
anova_nse <- aov(SINTOMAS_ANSIEDAD_ESTADO ~ NSE_TERCILES, data = covid)
cat("\nANOVA - Ansiedad estado por NSE:\n")
print(summary(anova_nse))

# Correlaciones importantes
cat("\nCorrelaciones clave:\n")
cor_edad_depresion <- cor(covid$EDAD, covid$SINTOMAS_DEPRESIVOS)
cor_dias_ansiedad <- cor(covid$DIAS_AISLAMIENTO, covid$SINTOMAS_ANSIEDAD_ESTADO)
cat("Edad vs Depresión:", round(cor_edad_depresion, 3), "\n")
cat("Días aislamiento vs Ansiedad:", round(cor_dias_ansiedad, 3), "\n")

# ===============================================
# 6. MODELOS PREDICTIVOS SIMPLES
# ===============================================

cat("\n=== MODELOS PREDICTIVOS ===\n")

# Modelo de regresión para síntomas depresivos
modelo_depresion <- lm(SINTOMAS_DEPRESIVOS ~ SEXO + EDAD + NSE_TERCILES + 
                      DIAS_AISLAMIENTO + TIPO_AISLAMIENTO, data = covid)
cat("Modelo de regresión - Síntomas Depresivos:\n")
print(summary(modelo_depresion))

# Modelo para ansiedad estado
modelo_ansiedad <- lm(SINTOMAS_ANSIEDAD_ESTADO ~ SEXO + EDAD + NSE_TERCILES + 
                     DIAS_AISLAMIENTO + MIEDO_RETORNO, data = covid)
cat("\nModelo de regresión - Ansiedad Estado:\n")
print(summary(modelo_ansiedad))

# ===============================================
# 7. ANÁLISIS DE CLUSTERS (PERFILES)
# ===============================================

cat("\n=== ANÁLISIS DE CLUSTERS ===\n")

# Preparar datos para clustering
datos_cluster <- covid %>%
  select(EDAD, DIAS_AISLAMIENTO, SINTOMAS_DEPRESIVOS, 
         SINTOMAS_ANSIEDAD_ESTADO, SINTOMAS_ANSIEDAD_RASGO) %>%
  scale()

# K-means con 3 clusters
set.seed(123)
kmeans_result <- kmeans(datos_cluster, centers = 3, nstart = 25)

# Agregar clusters al dataset original
covid$cluster <- as.factor(kmeans_result$cluster)

# Visualizar clusters
p14 <- ggplot(covid, aes(x = SINTOMAS_DEPRESIVOS, y = SINTOMAS_ANSIEDAD_ESTADO, color = cluster)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(title = "Clusters de Perfiles Psicológicos", 
       x = "Síntomas Depresivos", y = "Ansiedad Estado") +
  theme_minimal() +
  scale_color_manual(values = c("#3498db", "#e74c3c", "#2ecc71"))

print(p14)

# Características de cada cluster
cluster_summary <- covid %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    edad_promedio = round(mean(EDAD), 1),
    dias_aislamiento = round(mean(DIAS_AISLAMIENTO), 1),
    depresion_promedio = round(mean(SINTOMAS_DEPRESIVOS), 1),
    ansiedad_estado = round(mean(SINTOMAS_ANSIEDAD_ESTADO), 1),
    ansiedad_rasgo = round(mean(SINTOMAS_ANSIEDAD_RASGO), 1),
    pct_mujeres = round(sum(SEXO == "Mujer")/n()*100, 1)
  )

cat("\nCaracterísticas de los clusters:\n")
print(cluster_summary)

cat("\n=== ANÁLISIS COMPLETADO ===\n")
cat("Este script proporciona un análisis completo del impacto psicológico del COVID-19\n")
cat("Incluye: EDA, análisis comparativos, modelos predictivos y clustering\n")