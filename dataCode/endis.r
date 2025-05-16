# library(haven)
# 
# endis=read_sav("../data/endis/BASE NIÑOS ENDIS 2018 TERCEROS.sav")
# endis=as_factor(endis)
# variables = c("Edad_meses","Región","E27","PER_DISCAPACIDAD","E238",
#               "E239","IH10_1","IH11","IH4_NE_1","IH4_NE_2","IH4_NE_3","IH4_NE_11","E559","E560_1","E560_2","E59","IH22","IH26","IH28","IH32",
#               "EM1","EM5_NE","EM12","EM14","EM15","EM8_NE_1","EM8_NE_2","EM8_NE_3",
#               "EM22","EM23","EM27","EM9_NE","EM10_NE_7","EM11_NE",
#               "SI1_NE","SI17",
#               "SI6","SI8","SI19_NEE","SI20_NE_1_1","SI12",
#               "AL2","AL3","AL4","AL9","AL11_NV","AL15","AL16","AL17","AL24","AL26_NV",
#               "PC20","PC3_NE_1","PC8_NE_1","PC6_NE_2","EP1_NE","EP3_NE","CNV_edadm")
# endis=endis[,variables]
# endis = endis %>% filter(PC3_NE_1<30,SI20_NE_1_1<100,PC8_NE_1<40)
# 
# save(endis,file="./data/endis.RData",version=2)

#cargar datos que procesó Clementina -------------

library(tidyverse)
library(readxl)

endis = read_xlsx("../data/endis/cse_endis_informante_subset.xlsx")
diccionario = read_tsv("../data/endis/dict_endis.tsv",col_names = F)
colnames(diccionario)=c("var","label")


## cambiar niveles --------

niveles_ap  = list("Muy en desacuerdo","Ligeramente en desacuerdo","Ni en desacuerdo ni de acuerdo","Ligeramente de acuerdo","Muy de acuerdo")

niveles_bs = list("Si","No")

niveles_pc = list("Si","No","Ns/Nc")



endis = endis |> 
  mutate(across(starts_with("AP"),
                ~ factor(.x, levels = 1:5, labels = niveles_ap))) |> 
  mutate(across(starts_with("BS"),
                ~ factor(.x, levels = 1:2, labels = niveles_bs))) |> 
  mutate(across(starts_with("PC"),
                ~ factor(.x, levels = c(1,2,99), labels = niveles_pc))) 


#asignar etiquetas
add_labels <- function(df, diccionario) {
  for (i in seq_along(diccionario$var)) {
    col <- diccionario$var[i]
    label <- diccionario$label[i]
    attributes(df[[col]])$label <- label
  }
  df
}
endis <- add_labels(endis, diccionario)

attr(endis$PC1_NE_3,"label")




save(endis,file="./data/endis.RData",version=2)

