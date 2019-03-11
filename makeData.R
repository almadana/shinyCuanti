library(foreign)
library(dplyr)
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
