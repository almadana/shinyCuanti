# Exploración inicial del dataset COVID
# =====================================================

# Cargar librerías necesarias
library(tidyverse)
library(VIM)        # Para análisis de valores faltantes
library(corrplot)   # Para matriz de correlaciones
library(ggplot2)
library(plotly)     # Para gráficos interactivos

# Leer el archivo RData
load("data/covid.RData")

# =====================================================
# 1. EXPLORACIÓN BÁSICA DE LA ESTRUCTURA
# =====================================================

cat("=== EXPLORACIÓN INICIAL DEL DATASET ===\n\n")

# Listar objetos en el workspace
cat("Objetos cargados desde el archivo .RData:\n")
print(ls())

# Si hay múltiples objetos, examinar cada uno
for(obj_name in ls()) {
  cat("\n--- Objeto:", obj_name, "---\n")
  obj <- get(obj_name)
  
  cat("Clase:", class(obj), "\n")
  
  if(is.data.frame(obj)) {
    cat("Dimensiones:", nrow(obj), "filas x", ncol(obj), "columnas\n")
    cat("Primeras filas:\n")
    print(head(obj))
    
    cat("\nEstructura del dataset:\n")
    str(obj)
    
    cat("\nResumen estadístico:\n")
    print(summary(obj))
    
    cat("\nNombres de las columnas:\n")
    print(names(obj))
    
    cat("\nTipos de datos por columna:\n")
    print(sapply(obj, class))
    
    # Análisis de valores faltantes
    cat("\nValores faltantes por columna:\n")
    missing_vals <- colSums(is.na(obj))
    print(missing_vals[missing_vals > 0])
    
    if(sum(missing_vals) > 0) {
      cat("\nPorcentaje de valores faltantes:\n")
      missing_pct <- round(missing_vals/nrow(obj) * 100, 2)
      print(missing_pct[missing_pct > 0])
    }
  }
}

# =====================================================
# 2. IDENTIFICAR EL DATASET PRINCIPAL
# =====================================================

# Buscar el dataset principal (generalmente el más grande)
datasets <- list()
for(obj_name in ls()) {
  obj <- get(obj_name)
  if(is.data.frame(obj)) {
    datasets[[obj_name]] <- obj
  }
}

if(length(datasets) > 0) {
  # Seleccionar el dataset más grande como principal
  main_dataset_name <- names(datasets)[which.max(sapply(datasets, nrow))]
  main_dataset <- datasets[[main_dataset_name]]
  
  cat("\n=== ANÁLISIS DETALLADO DEL DATASET PRINCIPAL ===")
  cat("\nDataset principal identificado:", main_dataset_name)
  cat("\nDimensiones:", nrow(main_dataset), "x", ncol(main_dataset), "\n")
  
  # =====================================================
  # 3. ANÁLISIS DE VARIABLES
  # =====================================================
  
  # Separar variables por tipo
  numeric_vars <- names(main_dataset)[sapply(main_dataset, is.numeric)]
  character_vars <- names(main_dataset)[sapply(main_dataset, is.character)]
  factor_vars <- names(main_dataset)[sapply(main_dataset, is.factor)]
  date_vars <- names(main_dataset)[sapply(main_dataset, function(x) inherits(x, "Date") || inherits(x, "POSIXt"))]
  
  cat("\nVariables numéricas (", length(numeric_vars), "):\n")
  print(numeric_vars)
  
  cat("\nVariables categóricas/texto (", length(character_vars), "):\n")
  print(character_vars)
  
  cat("\nVariables factor (", length(factor_vars), "):\n")
  print(factor_vars)
  
  cat("\nVariables de fecha (", length(date_vars), "):\n")
  print(date_vars)
  
  # =====================================================
  # 4. ESTADÍSTICAS DESCRIPTIVAS
  # =====================================================
  
  if(length(numeric_vars) > 0) {
    cat("\n=== ESTADÍSTICAS DESCRIPTIVAS VARIABLES NUMÉRICAS ===\n")
    print(summary(main_dataset[numeric_vars]))
    
    # Detectar posibles outliers
    cat("\nDetección de outliers (valores fuera de Q1-1.5*IQR y Q3+1.5*IQR):\n")
    for(var in numeric_vars) {
      if(!all(is.na(main_dataset[[var]]))) {
        q1 <- quantile(main_dataset[[var]], 0.25, na.rm = TRUE)
        q3 <- quantile(main_dataset[[var]], 0.75, na.rm = TRUE)
        iqr <- q3 - q1
        outliers <- sum(main_dataset[[var]] < (q1 - 1.5*iqr) | 
                       main_dataset[[var]] > (q3 + 1.5*iqr), na.rm = TRUE)
        if(outliers > 0) {
          cat(var, ":", outliers, "outliers potenciales\n")
        }
      }
    }
  }
  
  # Análisis de variables categóricas
  if(length(character_vars) > 0 || length(factor_vars) > 0) {
    cat("\n=== ANÁLISIS VARIABLES CATEGÓRICAS ===\n")
    all_cat_vars <- c(character_vars, factor_vars)
    
    for(var in all_cat_vars) {
      if(length(unique(main_dataset[[var]])) <= 20) {  # Solo si no tiene demasiadas categorías
        cat("\nDistribución de", var, ":\n")
        print(table(main_dataset[[var]], useNA = "ifany"))
      } else {
        cat("\nVariable", var, "tiene", length(unique(main_dataset[[var]])), "valores únicos\n")
      }
    }
  }
}

cat("\n=== EXPLORACIÓN COMPLETADA ===\n")
cat("Ejecuta este script para ver la estructura de tus datos y luego podremos sugerir análisis específicos.\n")