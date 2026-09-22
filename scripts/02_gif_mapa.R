# Genera el GIF de portada: reclamos por CCZ, cambiando de área.
#
# El remate es Áreas Verdes: se da vuelta el mapa. Limpieza, Alumbrado y
# Saneamiento concentran en el este (CCZ 9, 11, 13); Áreas Verdes se mueve
# al centro-costero (CCZ 2, 3, 5), donde hay más espacio verde para reclamar.
#
# Entradas : data/app/ccz.rds, data/app/reclamos.parquet
# Salida   : docs/img/mapa_ccz.gif   (requiere ImageMagick para el ensamblado)

suppressPackageStartupMessages({
  library(sf); library(dplyr); library(arrow); library(data.table)
})

OUT <- "docs/img"
TMP <- file.path(tempdir(), "frames_reclamos")
dir.create(TMP, showWarnings = FALSE, recursive = TRUE)
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

geo <- readRDS("data/app/ccz.rds") |> st_transform(32721)
d   <- as.data.table(read_parquet("data/app/reclamos.parquet"))
d   <- d[!is.na(ccz)]

BG <- "#0B1220"; FG <- "#EAF0F8"; MUTED <- "#93A6C2"; FAINT <- "#42566F"
PAL <- c("#FFFFCC", "#FFE9A3", "#FED976", "#FEB24C", "#FD8D3C",
         "#FC4E2A", "#E31A1C", "#B10026")

VISTAS <- list(
  list(key = NULL,           titulo = "Todos los reclamos",
       nota = "1,7 millones desde 2010, geolocalizados por CCZ"),
  list(key = "Limpieza",     titulo = "Limpieza",
       nota = "Casi la mitad de todo: 806.161 reclamos, el 47%"),
  list(key = "Alumbrado",    titulo = "Alumbrado",
       nota = "Segunda en volumen, concentrada en el este de la ciudad"),
  list(key = "Saneamiento",  titulo = "Saneamiento",
       nota = "Tercera en volumen, con el mismo patrón que Alumbrado"),
  list(key = "Areas Verdes", titulo = "Áreas Verdes",
       nota = "Se da vuelta el mapa: manda el centro y la costa, no el este")
)

bbox <- st_bbox(geo)
asp  <- diff(bbox[c(1, 3)]) / diff(bbox[c(2, 4)])

png_frame <- function(v, archivo) {
  dd <- if (is.null(v$key)) d else d[area == v$key]
  cnt <- dd[, .(n = .N), by = .(ccz = as.character(ccz))]
  g <- merge(geo, cnt, by = "ccz", all.x = TRUE)
  g$n[is.na(g$n)] <- 0

  cortes <- unique(quantile(g$n, probs = seq(0, 1, length.out = 9), na.rm = TRUE))
  idx <- if (length(cortes) > 2) {
    as.integer(cut(g$n, breaks = cortes, include.lowest = TRUE))
  } else {
    rep(1L, nrow(g))
  }
  cols <- PAL[pmin(pmax(idx, 1), length(PAL))]

  png(archivo, width = 1080, height = 1350, res = 110)
  op <- par(bg = BG, mar = c(0, 0, 0, 0), xpd = NA)
  plot.new(); plot.window(xlim = c(0, 1080), ylim = c(1350, 0))
  rect(-10, -10, 1090, 1360, col = BG, border = NA)

  text(70, 88,  "Reclamos ciudadanos por zona", col = FG, cex = 2.35,
       font = 2, adj = 0)
  text(70, 132, "Montevideo (SUR) · 2010–2026 · 18 CCZ · 1,7 M de reclamos",
       col = MUTED, cex = 1.28, adj = 0)

  cex_tit <- if (nchar(v$titulo) > 16) 2.6 else 3.1
  text(70, 232, v$titulo, col = FG, cex = cex_tit, font = 2, adj = 0)
  text(70, 278, v$nota,   col = MUTED, cex = 1.3, adj = 0)

  total <- sum(g$n)
  text(1010, 232, format(total, big.mark = "."), col = FG, cex = 2.0,
       font = 2, adj = 1)
  text(1010, 268, "reclamos", col = FAINT, cex = 1.2, adj = 1)

  top <- 330; bot <- 1180
  alto <- bot - top; ancho <- alto * asp
  izq <- (1080 - ancho) / 2

  esc <- function(m) {
    x <- izq + (m[, 1] - bbox[1]) / diff(bbox[c(1, 3)]) * ancho
    y <- top + (bbox[4] - m[, 2]) / diff(bbox[c(2, 4)]) * alto
    cbind(x, y)
  }
  for (i in seq_len(nrow(g))) {
    gm <- st_geometry(g)[[i]]
    polys <- if (inherits(gm, "MULTIPOLYGON")) unlist(gm, recursive = FALSE) else gm
    for (p in polys) {
      m <- if (is.list(p)) p[[1]] else p
      if (!is.matrix(m) || nrow(m) < 3) next
      pt <- esc(m)
      polygon(pt[, 1], pt[, 2], col = cols[i], border = "#0B1220", lwd = 0.6)
    }
  }

  lx <- 70; ly <- 1232; w <- 46; h <- 16
  for (k in seq_along(PAL)) {
    rect(lx + (k - 1) * w, ly, lx + k * w, ly + h, col = PAL[k], border = NA)
  }
  text(lx, ly - 12, "menos", col = FAINT, cex = 1.0, adj = 0)
  text(lx + 8 * w, ly - 12, "más", col = FAINT, cex = 1.0, adj = 1)
  text(lx + 8 * w + 18, ly + h - 2, "escala por octiles", col = FAINT,
       cex = 1.0, adj = 0)

  segments(70, 1272, 1010, 1272, col = "#1C2A3E", lwd = 1.2)
  text(70, 1302, "Fuente: Intendencia de Montevideo · Sistema Único de Reclamos",
       col = MUTED, cex = 1.12, adj = 0)
  text(70, 1332, "emilianogonzalez.shinyapps.io/reclamos-montevideo", col = "#5E84B8",
       cex = 1.12, adj = 0)
  par(op); dev.off()
}

for (i in seq_along(VISTAS)) {
  f <- sprintf("%s/f%02d.png", TMP, i)
  png_frame(VISTAS[[i]], f)
  cat("cuadro", i, "-", VISTAS[[i]]$titulo, "\n")
}

# Áreas Verdes es el remate: se da vuelta el mapa, queda más tiempo en pantalla.
demoras <- c(240, 200, 200, 200, 360)
cuadros <- sprintf("%s/f%02d.png", TMP, seq_along(VISTAS))
destino <- file.path(OUT, "mapa_ccz.gif")
args <- c("-loop", "0",
          as.vector(rbind(paste0("-delay ", demoras), cuadros)),
          "-layers", "Optimize", destino)
system2("convert", args)
cat("\n", destino, " ", round(file.size(destino) / 1e6, 2), " MB\n", sep = "")
