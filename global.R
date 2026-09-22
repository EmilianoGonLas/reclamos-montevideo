# ============================================================
# Carga de librerías y datos globales
# ============================================================
#
# Los datos llegan pre-convertidos por scripts/01_preparar_datos.R: el CSV
# crudo de reclamos (303 MB) se guarda como parquet evento a evento (~pocos
# MB) con el Centro Comunal Zonal y el Municipio ya asignados por unión
# espacial. La app conserva el detalle completo y arranca en segundos.

required_pkgs <- c(
  "shiny", "bslib", "data.table", "dplyr", "dtplyr", "plotly",
  "DT", "arrow", "leaflet", "sf", "shinycssloaders",
  "viridisLite", "htmltools"
)
for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cran.r-project.org")
  }
}

library(shiny)
library(bslib)
library(data.table)
library(dplyr)
library(dtplyr)
library(plotly)
library(DT)
library(arrow)
library(leaflet)
library(sf)
library(shinycssloaders)
library(viridisLite)
library(htmltools)

APP_DATA <- "data/app"

# ============================================================
# DATOS
# ============================================================

reclamos_dt <- as.data.table(read_parquet(file.path(APP_DATA, "reclamos.parquet")))
setkey(reclamos_dt, anio, area)

ccz_sf        <- readRDS(file.path(APP_DATA, "ccz.rds"))
municipios_sf <- readRDS(file.path(APP_DATA, "municipios.rds"))

# --- Mapa base ---------------------------------------------------------------
# No se usa addProviderTiles(): la version actual de leaflet.providers apunta
# CartoDB a basemaps.carto.com, y CARTO paso a exigir API key en todos sus
# hosts. Esri Dark Gray Canvas es gratuito, no pide key y combina con el tema
# oscuro. Va solo el fondo, sin la capa de etiquetas, para no competir con el
# color de cada zona.
ESRI_FONDO <- paste0("https://services.arcgisonline.com/ArcGIS/rest/services",
                     "/Canvas/World_Dark_Gray_Base/MapServer/tile/{z}/{y}/{x}")
ESRI_ATTR <- paste(
  'Tiles &copy; <a href="https://www.esri.com/">Esri</a>',
  '&mdash; Esri, DeLorme, NAVTEQ'
)

agregar_mapa_base <- function(mapa) {
  leaflet::addTiles(mapa, urlTemplate = ESRI_FONDO, attribution = ESRI_ATTR,
                    options = leaflet::tileOptions(maxZoom = 17))
}

# ============================================================
# VARIABLES GLOBALES PARA FILTROS
# ============================================================
anios_reclamos  <- sort(unique(reclamos_dt$anio))
areas_reclamos  <- sort(unique(as.character(reclamos_dt$area)))
estados_reclamos <- c("Finalizado", "En Proceso", "Ingresado", "Anulado")
estados_reclamos <- estados_reclamos[estados_reclamos %in% levels(reclamos_dt$estado)]

ccz_ids        <- sort(unique(as.character(reclamos_dt$ccz)), na.last = NA)
municipio_ids  <- sort(unique(as.character(reclamos_dt$municipio)), na.last = NA)

# Grupo y tipo de problema, anidados bajo área: para que el selector de tipo
# sólo ofrezca lo que existe dentro del área elegida.
tipos_por_area <- reclamos_dt[, .(tipo_problema = sort(unique(as.character(tipo_problema)))),
                              by = area]
setkey(tipos_por_area, area)

dias_orden <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")
meses_orden <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")

# El mes corriente no está en los datos: la fuente publica "hasta el último
# día del mes anterior al corriente" y se sobrescribe cada mes.
fecha_max <- max(reclamos_dt$fecha_ingreso, na.rm = TRUE)
anio_max  <- max(anios_reclamos)
mes_incompleto_txt <- sprintf("Los datos llegan al %s.", format(fecha_max, "%d/%m/%Y"))

# --- Paleta categórica por área ---------------------------------------------
# Un tono por área, elegidos para distinguirse entre sí y aguantar el fondo
# oscuro. Las dos áreas casi vacías (Desarrollo Social: 1 caso; Operativa
# Municipios: apenas 182) van en gris para no competir por atención.
colores_area <- c(
  "Limpieza"              = "#3b82f6",
  "Alumbrado"              = "#f59e0b",
  "Saneamiento"            = "#22d3ee",
  "Areas Verdes"           = "#34d399",
  "Calles y veredas"       = "#a78bfa",
  "Barometrica"            = "#fb923c",
  "Transporte"             = "#f472b6",
  "CECOED"                 = "#ef4444",
  "Salubridad"             = "#a3e635",
  "Espacios Públicos" = "#38bdf8",
  "Gestión Ambiental" = "#6366f1",
  "Operativa Municipios"   = "#94a3b8",
  "Desarrollo Social"      = "#64748b"
)
colores_area <- colores_area[names(colores_area) %in% areas_reclamos]
color_area_default <- "#7f8c8d"

color_area <- function(a) {
  ifelse(a %in% names(colores_area), colores_area[a], color_area_default)
}

# Etiquetas cortas para CCZ / Municipio en mapas y tablas.
ccz_label <- setNames(paste("CCZ", ccz_ids), ccz_ids)
municipio_label <- setNames(paste("Municipio", municipio_ids), municipio_ids)

plot_bg_color   <- "transparent"
paper_bg_color  <- "transparent"
font_color_dark <- "#94a3b8"
grid_color_dark <- "#334155"

# ============================================================
# AYUDAS PARA PANTALLAS ANGOSTAS
# ============================================================
# El ancho llega como input global `ancho_px` (lo manda un script en app.R).
BREAKPOINT_MOVIL <- 768

es_angosto <- function(ancho) {
  !is.null(ancho) && !is.na(ancho) && ancho < BREAKPOINT_MOVIL
}

margen_eje <- function(ancho, deseado) {
  if (is.null(ancho) || is.na(ancho)) return(deseado)
  max(55, min(deseado, floor(ancho * 0.42)))
}

top_n_barras <- function(ancho) if (es_angosto(ancho)) 12L else 25L

# Formato uruguayo: punto de miles, coma decimal.
fnum <- function(x, decimales = 0) {
  format(round(x, decimales), big.mark = ".", decimal.mark = ",", nsmall = decimales,
         scientific = FALSE, trim = TRUE)
}
