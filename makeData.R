library(foreign)
library(tidyverse)
library(haven)
library(lubridate)
#dt=read.spss('../data/darkTriad/Triada Obscura.sav',to.data.frame = T)
dt=read_sav('../data/darkTriad/Triada Obscura.sav')
dt=as_factor(dt)
View(dt)
sum(is.na(dt$Refleja2))
dt=dt[complete.cases(dt),]
save(dt,file='./data/darkTriad.RData')

serce=read.spss('../data/serce/SERCE - Uruguay - sin missing.sav',to.data.frame = T)
serce2=read_sav('../data/serce/SERCE - Uruguay - sin missing.sav')
serce2=as_factor(serce2)
View(serce2)
serce=serce2
save(serce,file="./data/serce.RData")

load('../censo.RData')

colnames(censo)

censo$Num_hijos



##--- etiquetar niveles de variables ----
censo$Lugar_Nac = factor(censo$Lugar_Nac)
levels(censo$Lugar_Nac) = c("Uruguay","Exterior")
censo$Dep_Nac = factor(censo$Dep_Nac)
levels(censo$Dep_Nac) = c("Maldonado","Paysandú","Río Negro","Rivera","Rocha","Salto","San José","Soriano",
                          "Tacuarembó","Treinta y Tres","Montevideo","Artigas","Canelones","Cerro Largo","Colonia","Durazno",
                          "Flores","Florida","Lavalleja")

levels(censo$País) = c("Uruguay","Argentina","Aruba","Bélgica","Bolivia","Brasil","Canadá","Suiza",
                       "Chile","Colombia","Costa Rica","Cuba","Alemania","Rep. Dominicana","Egipto","España","Francia","Israel","Italia","México",
                       "Panamá","Portugal","Paraguay","Rumania","Suecia","Ucrania","EE.UU.","Venezuela")


censo$Nro_trabajos = censo$Nro_trabajos -240

censo$Posición_Ocupacional = factor(censo$Posición_Ocupacional)
levels(censo$Posición_Ocupacional) = c("Directivo o Gerente ","Profesionales y Técnicos","Socio de establecimiento industrial/rural","Socio de Comercio y/o servicio ","Docente / Investigador",
"Personal de apoyo administrativo y de oficina","Trabajadores de los servicios y vendedores de comercio","Oficiales, operarios y otros oficios","Trabajador Independiente / Ejercicio liberal","Educador",
"Trabajador Rural","Miembro de las Fuerzas Armadas","Percibe rentas y/o intereses ","Trabajador no remunerado ","Otra ocupación")


censo$Único_percibe_ingresos_hogar = factor(censo$Único_percibe_ingresos_hogar)
levels(censo$Único_percibe_ingresos_hogar)=c("Si","No")

censo$Área_inser_1=factor(censo$Área_inser_1)
levels(censo$Área_inser_1)=c("Educación (psicopedagogía y problemas de aprendizaje)","Educación (psicología institucional o familiar).","Educación (psicodiagnóstico y psicometría)","Educación (orientación vocacional)","Social-Comunitario público (MIDES, Intendencias u otra institución)","Social-Comunitario privado (ONG, asociaciones civiles, etc)","Institucional-organizacional (RRHH)","Institucional-organizacional (otro)","Consultorías privadas (mercadeo, asesoramiento  a organizaciones internacionales,   asesoramiento a ONG)","Clínica privada (psicoterapia)","Clínica privada (psicodiagnóstico y evaluación)","Clínica privada: (otro)","Salud mental público (primer nivel)","Salud mental público (segundo nivel)","Salud mental público (tercer nivel)","Salud mental privado (primer nivel)","Salud mental privado (segundo nivel)","Salud mental privado (tercer nivel)","Salud (Cuidados paliativos, profilaxis, otros)","Docencia Universidad pública (docencia, investigación, extensión)","Docencia Universidad privada (docencia, investigación)","Docencia secundaria pública","Docencia secundaria privada","Docencia en otras instituciones (organizaciones científicas, etc.)","Subsector privado del mutualismo y seguros privados","Subsector público ASSE","Judicial (Forense)","Psicología Política","Psicología del Deporte","Psicología Ambiental","Publicidad y marketing")

censo$Vivienda_Ud_es=factor(censo$Vivienda_Ud_es)
levels(censo$Vivienda_Ud_es)=c("Propietario y la está pagando","Propietario y ya la pagó","Miembro de cooperativa de ayuda mutua","Inquilino o arrendatario","Ocupante gratuito (se la prestaron) ","Ocupante sin permiso del propietario","Por relación de dependencia (trabajo)","Otro")

censoFil=censo %>% 
  select(Sexo,Edad,Lugar_Nac,Dep_Nac,País,Nro_trabajos,Estado_conyugal,Num_hijos,Posición_Ocupacional,Único_percibe_ingresos_hogar,Área_inser_1,Vivienda_Ud_es)
sum( complete.cases(censoFil))


censoFil = censoFil[complete.cases(censoFil),]
View(censoFil)
save(censoFil,file="./data/censo.RData")

load("./data/censo.RData")
table(censoFil$Sexo,censoFil$Estado_conyugal)



#encuestaCuanti
encuesta=read.csv("./data/encuestaCuanti.csv")
head(encuesta)
colnames(encuesta)=c("timestamp","Sexo","Edad","Dep.residencia","Barrio.vive","Liceo","Trabaja","Salario","Edad.meses","ESV1","ESV2","ESV3","ESV4","ESV5","N.Personas.vive","Horas.sueño","ud.es")
levels(encuesta$Sexo)=c("Varón","Mujer","Otra@")
levels(encuesta$Liceo)=c("Público","Privado")
levels(encuesta$Trabaja)=c("Sí","No","Ns/nc")
levels(encuesta$Salario)=c("Sin salario","Entre 1 y 4.999","Entre 5.000 y 9.999","Entre 10.000 y 29.999","Más de 30.000","Ns/nc")
levels(encuesta$Horas.sueño)



encuesta$RangoEdad=sapply(encuesta$Edad,function(x) {ifelse(x<25,"Menos de 25 años",ifelse(x<30, "Entre 26 y 30 años",ifelse(x<35,"Entre 31 y 34 años", "35 años o más" ) ) )})
encuesta$RangoEdad = factor(encuesta$RangoEdad)

encuesta$horas.sueño.hms = hms(encuesta$Horas.sueño)

horaMal = (encuesta$horas.sueño.hms < hms("2:00:00")) | (encuesta$horas.sueño.hms > hms("15:00:00"))
sum(horaMal)

encuesta = encuesta[!horaMal,]
encuesta$horas.sueño=round ( period_to_seconds(encuesta$horas.sueño.hms) / 3600 , 2) # pasar a segundos, luego a horas, y luego redondear resultado


encuesta =encuesta %>% 
  mutate(Escala.satisfaccion.vida = rowSums(select(.,contains("ESV"))) )

colnames(encuesta)


encuesta1=encuesta[,c(2,3,4,6,7,8,15,18,20,21)]
table(encuesta1$RangoEdad,encuesta1$Trabaja)
save(encuesta1,file="./data/encuesta.RData")


###--- Latinobarómetro----
library(haven)
#latino=read.spss("../data/latinobarometro/Latinobarometro2017Abrev.sav",to.data.frame = T)
latino=read_sav("../data/latinobarometro/Latinobarometro2017Abrev.sav")
latino=as_factor(latino)
latino$CIUDAD=droplevels(latino$CIUDAD)
str(latino)
save(latino,file="./data/latino.RData")


#### -- MINI BASE DE DATOS ESTUDIANTES ----
miniBase = read.csv("../minibase.csv",dec=",")
head(miniBase)
save(miniBase,file="./data/miniBase.RData")


## - music babies ----



music = read.csv(file = "../data/musicBabies/Mehr Song and Spelke 2016 Experiment 1.csv")
View(music)

music1 = music %>% 
  filter(exp1==1) %>% 
  select("id","female","dad","Baseline_Proportion_Gaze_to_Singer","Test_Proportion_Gaze_to_Singer","Difference_in_Proportion_Looking","age","Estimated_Total_Number_of_Song")
View(music1)

colnames(music1)
colnames(music1) = c("Id","Es.mujer","Vino.con.el.padre","Prop.Mirada.Canta.Base","Prop.Mirada.Canta.Test","Dif.Prop.Mirada","edad","Num.canc.estim")
attributes(music1$Id)$label = "Identificador del bebé"
attributes(music1$Mujer)$label = "Es mujer?"
attributes(music1$Vino.con.el.padre)$label = "¿Vino con el padre?"
attributes(music1$Prop.Mirada.Canta.Base)$label = "Proporción de tiempo que mira a quien (luego) canta la canción conocida. Línea de base"
attributes(music1$Prop.Mirada.Canta.Test)$label = "Proporción de tiempo que mira a quien canta la canción conocida. Test."
attributes(music1$Dif.Prop.Mirada)$label = "Diferencia en la proporción de tiempo que mira a quien canta la canción conocida, Test menos línea de base."
attributes(music1$edad)$label = "Edad en meses"
attributes(music1$Num.canc.estim)$label = "Número estimado de veces que el bebé escuchó la canción de parte de sus cuidadores."

music1$Mujer = factor(music1$Mujer,labels = c("No","Si"))
music1$Vino.con.el.padre =  factor(music1$Vino.con.el.padre,labels = c("No","Si"))

save(music1,file="./data/expeCuna.RData")


#---- ricos y pobres----
wealth = read.csv("../data/wealth/Dawtry Sutton and Sibley 2015 Study 1a.csv")
head(wealth)

wealth$f_s=apply(wealth[,c("fairness","satisfaction")],1,mean)

wealth$redist2_r = 7 - wealth$redist2
wealth$redist4_r = 7 - wealth$redist4

wealth$redist=apply(wealth[,c("redist1","redist2_r","redist3","redist4_r")],1,mean)

plot(wealth$Social_Circle_Mean_Income,wealth$redist)
cor.test(wealth$Social_Circle_Mean_Income,wealth$redist)
cor.test(wealth$redist,wealth$f_s)
plot(wealth$redist,wealth$f_s)

plot(wealth$f_s,wealth$redist)
abline(lm(wealth$redist~wealth$f_s))

plot(wealth$f_s,wealth$Household_Income)
cor.test(wealth$Household_Income,wealth$f_s)
abline(lm(wealth$Household_Income~wealth$f_s))


plot(wealth$redist,wealth$Household_Income)
cor.test(wealth$Household_Income,wealth$redist)
abline(lm(wealth$Household_Income~wealth$redist))

wealth2 = mutate(wealth,income3tile = ntile(Household_Income,3))
wealth2 = mutate(wealth2,political3tile = ntile(Political_Preference,3))

colnames(wealth2)
wealth3 = wealth2 %>% 
  select("PS","Household_Income","Population_Inequality_Gini_Index","Social_Circle_Inequality_Gini_Index","f_s","redist","income3tile","political3tile")
colnames(wealth3)

colnames(wealth3) = c("Sujeto","Ingreso.hogar","Gini_estimado_poblacion","Gini_estimado_circ.social",
                      "Igualdad_y_satisfaccion","Redistribucion","Tercil_ingreso_hogar","Orientacion_politica")

wealth3$Tercil_ingreso_hogar = factor(wealth3$Tercil_ingreso_hogar,labels=c("Menor","Medio","Mayor"))
wealth3$Orientacion_politica = factor(wealth3$Orientacion_politica,labels=c("Liberal-Izq","Centro","Conservador-Der"))

attributes(wealth3$Sujeto)$label="Número de participante"
attributes(wealth3$Ingreso.hogar)$label="Ingreso al hogar"
attributes(wealth3$Gini_estimado_poblacion)$label="Índice gini de la estimación de distribución de ingreso al hogar en la población de los EEUU"
attributes(wealth3$Gini_estimado_circ.social)$label="Índice gini de la estimación de distribución de ingreso al hogar en el círculo social del participante"
attributes(wealth3$Igualdad_y_satisfaccion)$label="Puntuación en la escala de percepción de justicia y satisfacción con la distribución de ingreso actual"
attributes(wealth3$Redistribucion)$label="Puntuación en la escala de acuerdo con la afirmación 'Creo que el estado debería redistribuir la riqueza a través de impuestos a los más ricos'"
attributes(wealth3$Tercil_ingreso_hogar)$label = "Nivel de ingreso al hogar (por debajo del primer tercil, entre el primer y segundo tercil, por encima del tercer tercil)"
attributes(wealth3$Orientacion_politica)$label = "Orientación política (liberal de izquierda, centro, conservador de derecha)"

wealth3=wealth3[complete.cases(wealth3),]

save(wealth3,file="./data/wealth.RData")
#------ inteligencia -----

inteli = read_sav("../data/inteligencia/alldata/Study1 data.sav")
inteli$gender.f = factor(inteli$gender)
levels(inteli$gender.f)


inteli4 = read.csv("../data/inteligencia/Schroeder and Epley 2015 Study 4 data.csv")
head(inteli4)
inteli4$cond.f = factor(inteli4$CONDITION,labels = c("Transcripción","Audio"))

boxplot(inteli4$Impression_Rating~inteli4$cond.f)

cor(inteli4$Impression_Rating,inteli4$Hire_Rating)

colnames(inteli4)
View(inteli4)


inteli =inteli4 %>% 
  select(cond.f,Intellect_Rating,Impression_Rating,Hire_Rating,wordcount,time)
colnames(inteli)
colnames(inteli)=c("Condición","Puntaje_intelecto","Puntaje_impresion_global","Puntaje_Contratar","num.palabras","tiempo")
attributes(inteli$Condición)$label = "Transcripción o audio"
attributes(inteli$Puntaje_intelecto)$label = "Puntuación general del intelecto del evaluado"
attributes(inteli$Puntaje_impresion_global)$label = "Puntuación de impresión global del evaluado"
attributes(inteli$Puntaje_Contratar)$label = "Puntuación de qué tan buen candidato para contratar es el evaluado"
attributes(inteli$num.palabras)$label = "Número de palabras de la presentación"
attributes(inteli$tiempo)$label = "Tiempo que pasó el evaluador evaluando el candidato"

save(inteli,file="./data/inteligencia.RData")


#-------------- World Survey Data --------------

library(readxl)

wvs = read_xlsx("../data/wvs/F00012990-WVS_Wave_7_Uruguay_Excel_v6.0.xlsx",na = c(""," ","NA","-2","-1"))
colnames(wvs)

selected_vars = c(54,55,86,87,99,100,101,103,104,115,209,218,219,220,224,274,276,279,289,291,299,301,317,328,335,336)
wvs = wvs |>  select(all_of(selected_vars))



## -- diccionario en inglés ----------
# dic_wvs = read_xlsx("../data/wvs/variables_wvs_uy_2022.xlsx",col_names = F)
# # 
# colnames(dic_wvs) = "variable"
# dic_wvs = dic_wvs[selected_vars,]
# dic_wvs = dic_wvs |> 
#   separate(variable,into = c("variable","dimension","description"),sep = ": ") |> 
#   unite(col = "variable",variable,dimension,sep=": ")
# 
# write.csv(dic_wvs,file = "../data/wvs/dic_english.csv")
# 

## -- diccinario en español ---------
dic_wvs_es = read.csv("../data/wvs/dic_es.csv")

colnames(wvs)
colnames(dic_wvs_es)
colnames(wvs) = dic_wvs_es$variable_espanol
dic_wvs_es

colnames(wvs)

vn = colnames(wvs)
i=1
dic_wvs_es$variable_espanol[i] %in% vn

add_labels <- function(df, diccionario) {
  variables_en_df = colnames(df)

  for (i in seq_along(diccionario$variable_espanol)) {
    if (diccionario$variable_espanol[i] %in% variables_en_df){
      col <- diccionario$variable_espanol[i]
      label <- diccionario$descripcion_espanol[i]
      attributes(df[[col]])$label <- label
    }
  }
  df
}

#wvs$pais = "UY"
#wvs$pais = as.factor(wvs$pais)


## etiquetas de niveles-----------
# wvs$`Q2 3: Vecinos` = as.factor(wvs$`Q23: Vecinos`)
# 
# levels(wvs$`Q23: Vecinos`) = c("No le gustaría","No menciona")
# 
# wvs$`Q24: Vecinos` = as.factor(wvs$`Q24: Vecinos`)
# levels(wvs$`Q24: Vecinos`) = c("No le gustaría","No menciona")

wvs = wvs |> mutate(across(contains("Vecinos"),.fns = function(x) {
  x.f = as.factor(x)
  levels(x.f) = c("No le gustaría","No menciona")
  return(x.f)
}))


wvs = wvs |> mutate(across(contains("Confianza"),.fns = function(x) {
  x.f = as.factor(x)
  levels(x.f) = c("Mucha","Algo","Poco","Nada")
  return(x.f)
}))

wvs = wvs |> mutate(across(contains("Frecuencia"),.fns = function(x) {
  x.f = as.factor(x)
  levels(x.f) = c("A menudo","A veces","Rara vez","Nunca")
  return(x.f)
}))
levels(as.factor(wvs$`Q182: Justificable – homosexualidad`))

# wvs = wvs |> mutate(across(contains("Justificable"),.fns = function(x) {
#   x.f = as.factor(x)
#   levels(x.f) = c("No contesta","No sabe","1 - Nunca",2:9," 10 - Siempre")
#   return(x.f)
# }))

wvs = wvs |> mutate(across(contains("Sistema",ignore.case = F),.fns = function(x) {
  x.f = as.factor(x)
  levels(x.f) = c("Muy bueno","Bueno","Malo","Muy malo")
  return(x.f)
}))

wvs = wvs |> mutate(across(contains("Es religios",ignore.case = F),.fns = function(x) {
  x.f = as.factor(x)
  levels(x.f) = c("Una persona religiosa","Una persona no religiosa","Un ateo")
  return(x.f)
}))

wvs$`Q237: Sistema político – gobierno militar`
wvs = wvs |> mutate(across(contains("Sistema pol",ignore.case = F),.fns = function(x) {
  x.f = as.factor(x)
  levels(x.f) = c("Muy bueno","Bueno","Malo","Muy malo")
  return(x.f)
}))

# wvs = wvs |> mutate(across(contains("Importancia de la democracia",ignore.case = F),.fns = function(x) {
#   x.f = as.factor(x)
#   levels(x.f) = c("No contesta","No sabe","1 - Nada importante",2:9," 10 - Absolutamente importante")
#   return(x.f)
# }))

# wvs = wvs |> mutate(across(contains("Satisfacción con sistema pol",ignore.case = F),.fns = function(x) {
#   x.f = as.factor(x)
#   levels(x.f) = c("No contesta","No sabe","1 - Completamente insatisfecho",2:9," 10 - Completamente satisfecho")
#   return(x.f)
# }))

wvs = wvs |> mutate(across(contains("Sexo",ignore.case = F),.fns = function(x) {
  x.f = as.factor(x)
  levels(x.f) = c("Hombre","Mujer")
  return(x.f)
}))


# wvs = wvs |> mutate(across(contains("Democracia",ignore.case = F),.fns = function(x) {
#   x.f = as.factor(x)
#   levels(x.f) = c("No contesta","No sabe","0 - En contra","1 - Izquierda",2:9," 10 - Derecha")
#   return(x.f)
# }))

wvs = wvs |> mutate(across(contains("Nivel educativo",ignore.case = F),.fns = function(x) {
  x.f = as.factor(str_replace(x,"^.*(\\d)$","\\1"))
  levels(x.f) = c(NA,"Educación inicial","Primaria","Media básica","Media superior","Terciaria técnica",
                  "Terciaria técnica","Grado universitario","Posgrado maestría","Posgrado doctorado")
  return(x.f)
}))


# wvs$`Q251: Qué tan democráticamente se gobierna este país hoy` = as.factor(wvs$`Q251: Qué tan democráticamente se gobierna este país hoy`)
# levels(wvs$`Q251: Qué tan democráticamente se gobierna este país hoy`) = 
#   c("No contesta","No sabe","1 - Nada",2:9," 10 - Completamente")
# 
# wvs$`Q253: Respeto por los derechos humanos en la actualidad` = as.factor(wvs$`Q253: Respeto por los derechos humanos en la actualidad`)
# levels(wvs$`Q253: Respeto por los derechos humanos en la actualidad`) = 
#   c("No contesta","No sabe","1 - Nada",2:9," 10 - Completamente")
# 
# 

wvs$`Q287: Clase social (subjetiva)` = as.factor(wvs$`Q287: Clase social (subjetiva)`)
levels(wvs$`Q287: Clase social (subjetiva)`) = 
   c("Clase alta","Clase media alta","Clase media baja","Clase obrera","Clase baja")


#colnames(wvs)
#removed_vars = c(11)
wvs = add_labels(wvs, dic_wvs_es)


save(wvs,file="./data/wvs.RData")

