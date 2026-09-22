# =============================================================================
# deploy.R — Publica/actualiza esta app en shinyapps.io
#
# NO contiene credenciales: usa la cuenta ya registrada en la máquina
# (rsconnect::setAccountInfo, guardada en la config local de rsconnect).
#
# El bundle no incluye el CSV crudo (303 MB) ni los shapefiles de origen: la
# app lee los derivados de data/app/ (parquet + rds). Si cambiaron los datos
# crudos, correr antes:
#
#   Rscript scripts/01_preparar_datos.R
#
# Uso:   Rscript scripts/deploy.R   (SIEMPRE desde la raíz del repo: appDir
#        = "." se resuelve contra el directorio de trabajo, no contra la
#        ubicación de este archivo)
# =============================================================================
rsconnect::deployApp(
  appDir   = ".",
  appFiles = c(
    "app.R", "global.R",
    "R/mod_mapa.R", "R/mod_tipos.R", "R/mod_tiempos.R", "R/mod_explorador.R",
    "data/app/reclamos.parquet",
    "data/app/ccz.rds",
    "data/app/municipios.rds"
  ),
  appName        = "reclamos-montevideo",
  account        = "emilianogonzalez",
  forceUpdate    = TRUE,
  launch.browser = FALSE
)
