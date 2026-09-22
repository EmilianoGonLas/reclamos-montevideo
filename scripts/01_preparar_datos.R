# ============================================================
# Prepara los datos de la app a partir del CSV crudo del catálogo
# ============================================================
#
# Entrada:
#   - data/raw/reclamos.csv (1.700.608 filas, 303 MB): catalogodatos.gub.uy,
#     dataset "reclamos-registrados-en-el-sistema-unico-de-reclamos-de-la-
#     intendencia-de-montevideo". Se sobrescribe cada mes con la situación
#     actual (no es un archivo append-only).
#   - data/shp/ccz/sig_comunales.shp y data/shp/municipios/sig_municipios.shp:
#     límites de Centros Comunales Zonales y Municipios, intgis.montevideo.gub.uy.
#
# Salida (data/app/, ~pocos MB, es lo que versiona el repo y lo que carga
# la app):
#   - reclamos.parquet: evento a evento, con CCZ y Municipio ya asignados
#     por unión espacial punto-en-polígono.
#   - ccz.rds / municipios.rds: geometría simplificada para leaflet.

library(data.table)
library(sf)
library(terra)
library(arrow)

# --- 1. Reclamos -------------------------------------------------------------
cat("Leyendo CSV crudo...\n")
dt <- fread("data/raw/reclamos.csv", encoding = "UTF-8")
setnames(dt, tolower(names(dt)))

dt[, fecha_ingreso := as.Date(fecha_ingreso_reclamo)]
dt[, fecha_estado  := as.Date(fecha_desde_en_estado)]
dt[, anio          := year(fecha_ingreso)]
dt[, anio_mes      := as.Date(format(fecha_ingreso, "%Y-%m-01"))]

dias_es <- c("Monday" = "Lunes", "Tuesday" = "Martes", "Wednesday" = "Miércoles",
             "Thursday" = "Jueves", "Friday" = "Viernes", "Saturday" = "Sábado",
             "Sunday" = "Domingo")
dt[, dia_semana := dias_es[weekdays(fecha_ingreso)]]

# Días entre el ingreso y el cierre. Sólo tiene sentido para "Finalizado":
# para el resto, fecha_desde_en_estado es la última transición registrada
# hoy, no un cierre, y el dato se sobrescribe cada mes con la situación
# vigente (no queda historial de estados intermedios).
dt[, dias_resolucion := as.numeric(fecha_estado - fecha_ingreso)]
dt[desc_estado != "Finalizado", dias_resolucion := NA_real_]
# Un puñado de fechas de alta posteriores a la de cierre (carga manual mal
# hecha en el origen) dan una resolución negativa: se descartan de esta
# métrica puntual sin tocar el resto del registro.
dt[dias_resolucion < 0, dias_resolucion := NA_real_]

# --- 2. Unión espacial: CCZ y Municipio --------------------------------------
cat("Uniendo con CCZ y Municipios (unión espacial punto-en-polígono)...\n")
coords <- as.matrix(dt[, .(longitud, latitud)])
puntos <- vect(coords, type = "points", crs = "EPSG:4326")

ccz_v        <- project(vect("data/shp/ccz/sig_comunales.shp"), "EPSG:4326")
municipios_v <- project(vect("data/shp/municipios/sig_municipios.shp"), "EPSG:4326")

dt[, ccz        := sub("^CCZ0?", "", extract(ccz_v, puntos)$ZONA_LEGAL)]
dt[, municipio  := extract(municipios_v, puntos)$MUNICIPIO]

sin_ccz <- dt[is.na(ccz), .N]
cat(sprintf("Reclamos sin CCZ asignado (coordenada fuera de los 18 polígonos): %d (%.2f%%)\n",
            sin_ccz, 100 * sin_ccz / nrow(dt)))

# --- 3. Columnas finales ------------------------------------------------------
app_dt <- dt[, .(
  numero_reclamo, fecha_ingreso, anio, anio_mes, dia_semana,
  estado = desc_estado, fecha_estado, dias_resolucion,
  area = desc_area, grupo = desc_grupo, tipo_problema = desc_tipoproblema,
  lat = latitud, lon = longitud,
  ccz, municipio
)]

for (col in c("estado", "area", "grupo", "tipo_problema", "ccz", "municipio", "dia_semana")) {
  set(app_dt, j = col, value = as.factor(app_dt[[col]]))
}

dir.create("data/app", showWarnings = FALSE, recursive = TRUE)
write_parquet(app_dt, "data/app/reclamos.parquet")
cat(sprintf("data/app/reclamos.parquet: %.1f MB\n",
            file.size("data/app/reclamos.parquet") / 1024^2))

# --- 4. Geometrías para el mapa ----------------------------------------------
# Simplificadas a 15 m *en la proyección UTM original* (metros, plana): el
# shapefile ya viene en UTM 21S, así que se simplifica ahí y recién después
# se pasa a 4326. Hacerlo al revés (simplificar en 4326 con el backend S2,
# que asume geometría esférica) generó un polígono con un vértice duplicado
# que S2 rechaza como inválido ("Edge ... is degenerate"). st_make_valid()
# antes de transformar, no después, porque S2 es más estricto que GEOS.
ccz_sf <- st_read("data/shp/ccz/sig_comunales.shp", quiet = TRUE) |>
  st_make_valid() |>
  st_simplify(dTolerance = 15) |>
  st_make_valid() |>
  st_transform(4326) |>
  dplyr::transmute(ccz = sub("^CCZ0?", "", ZONA_LEGAL))
saveRDS(ccz_sf, "data/app/ccz.rds")

municipios_sf <- st_read("data/shp/municipios/sig_municipios.shp", quiet = TRUE) |>
  st_make_valid() |>
  st_simplify(dTolerance = 15) |>
  st_make_valid() |>
  st_transform(4326) |>
  dplyr::transmute(municipio = MUNICIPIO)
saveRDS(municipios_sf, "data/app/municipios.rds")

cat("Listo.\n")
