library(dplyr)

load("data/censo.RData")
stopifnot(exists("censoFil"))

datos <- censoFil

#Agrupo los niveles de ocupación segun la clasificacion de la OIT
datos <- datos %>%
  mutate(ocupacion_grupo = factor(case_when(
    Posición_Ocupacional %in% c("Directivo o Gerente ",
                     "Profesionales y Técnicos",
                     "Docente / Investigador",
                     "Educador",
                     "Trabajador Independiente / Ejercicio liberal") ~ "Profesionales/Alta calificación",
    Posición_Ocupacional %in% c("Socio de Comercio y/o servicio ",
                     "Trabajadores de los servicios y vendedores de comercio",
                     "Personal de apoyo administrativo y de oficina") ~ "Servicios/Comercio",
    Posición_Ocupacional %in% c("Socio de establecimiento industrial/rural",
                     "Oficiales, operarios y otros oficios",
                     "Trabajador Rural") ~ "Industria/Rural",
    Posición_Ocupacional %in% c("Miembro de las Fuerzas Armadas",
                     "Percibe rentas y/o intereses ",
                     "Trabajador no remunerado ",
                     "Otra ocupación") ~ "Otros",
    TRUE ~ "Sin clasificar"
  )))

#Agrupo las areas de inserción
datos <- datos %>%
  mutate(
    `Área_inserción` = factor(case_when(
      Área_inser_1 %in% c(
        "Educación (psicopedagogía y problemas de aprendizaje)",
        "Educación (psicología institucional o familiar).",
        "Educación (psicodiagnóstico y psicometría)",
        "Educación (orientación vocacional)"
      ) ~ "Educación",
      Área_inser_1 %in% c(
        "Social-Comunitario público (MIDES, Intendencias u otra institución)",
        "Social-Comunitario privado (ONG, asociaciones civiles, etc)"
      ) ~ "Social-Comunitario",
      Área_inser_1 %in% c(
        "Institucional-organizacional (RRHH)",
        "Institucional-organizacional (otro)"
      ) ~ "Institucional",
      Área_inser_1 %in% c(
        "Clínica privada (psicoterapia)",
        "Clínica privada (psicodiagnóstico y evaluación)",
        "Clínica privada: (otro)",
        "Salud mental público (primer nivel)",
        "Salud mental público (segundo nivel)",
        "Salud mental público (tercer nivel)",
        "Salud mental privado (primer nivel)",
        "Salud mental privado (segundo nivel)",
        "Salud mental privado (tercer nivel)",
        "Salud (Cuidados paliativos, profilaxis, otros)",
        "Subsector privado del mutualismo y seguros privados",
        "Subsector público ASSE"
      ) ~ "Salud",
      Área_inser_1 %in% c(
        "Docencia Universidad pública (docencia, investigación, extensión)",
        "Docencia Universidad privada (docencia, investigación)",
        "Docencia secundaria pública",
        "Docencia secundaria privada",
        "Docencia en otras instituciones (organizaciones científicas, etc.)"
      ) ~ "Docencia",
      TRUE ~ as.character(Área_inser_1)
    ))
  ) %>%
  select(-Área_inser_1)

# Crear objeto final y guardar
censoRec <- datos
save(censoRec, file = "/home/usuario/shinyCuanti/data/censo_recod.RData")
write.csv(censoRec, "/home/usuario/shinyCuanti/data/censo_recod.csv", row.names = FALSE)
