verificar_paquetes_DT <- function() {
  paquetes <- c("DT", "htmlwidgets", "crosstalk", "jquerylib", "shiny")
  versiones_minimas <- c("0.31", "1.6.4", "1.2.1", "0.1.4", "1.10.0")
  
  for (i in seq_along(paquetes)) {
    p <- paquetes[i]
    v <- versiones_minimas[i]
    if (!requireNamespace(p, quietly = TRUE)) {
      message(sprintf("❌ Paquete %s no está instalado.", p))
    } else if (packageVersion(p) < v) {
      message(sprintf("⚠️  Paquete %s está en versión %s (se recomienda >= %s)", 
                      p, packageVersion(p), v))
    } else {
      message(sprintf("✅ %s %s OK", p, packageVersion(p)))
    }
  }
}
