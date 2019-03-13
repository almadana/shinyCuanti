library(foreign)
library(dplyr)
library(haven)
dt=read.spss('../data/darkTriad/Triada Obscura.sav',to.data.frame = T)
head(dt)
sum(is.na(dt$Refleja2))
dt=dt[complete.cases(dt),]
save(dt,file='./data/darkTriad.RData')

serce=read.spss('../data/serce/SERCE - Uruguay - sin missing.sav',to.data.frame = T)
save(serce,file="./data/serce.RData")

load('../censo.RData')

colnames(censo)

censo$Num_hijos

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


encuesta1=encuesta[,c(2,3,4,6,7,8,15,18)]
table(encuesta1$RangoEdad,encuesta1$Trabaja)


### Latinobarómetro
library(haven)
#latino=read.spss("../data/latinobarometro/Latinobarometro2017Abrev.sav",to.data.frame = T)
latino=read_sav("../data/latinobarometro/Latinobarometro2017Abrev.sav")
latino=as_factor(latino)
latino$CIUDAD=droplevels(latino$CIUDAD)
str(latino)
save(latino,file="./data/latino.RData")
