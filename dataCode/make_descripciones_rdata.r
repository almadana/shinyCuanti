# make RData con las descripciones en csv

descripciones = read.csv("data/descripciones_bases_panel.csv")
save(descripciones,file="data/descripciones.RData")

