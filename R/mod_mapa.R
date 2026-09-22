# ============================================================
# Módulo: Mapa por zona (CCZ / Municipio)
# ============================================================
# Los reclamos miden comportamiento de reporte, no incidencia objetiva del
# problema: una zona con más reclamos puede tener más problemas, o vecinos
# más propensos a reclamar (más acceso a la app, más tiempo, más costumbre).
# El mapa no pretende resolver esa ambigüedad, sólo mostrar dónde reclama la
# gente.

mod_mapa_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      title = "Filtros",
      width = 320,

      sliderInput(ns("periodo"), "Período (año de ingreso):",
                  min = min(anios_reclamos), max = max(anios_reclamos),
                  value = c(max(anios_reclamos) - 2, max(anios_reclamos)),
                  step = 1, sep = "", ticks = FALSE),

      selectizeInput(ns("areas"), "Área:",
                     choices = areas_reclamos, selected = areas_reclamos,
                     multiple = TRUE),

      checkboxGroupInput(ns("estados"), "Estado del reclamo:",
                         choices = estados_reclamos, selected = estados_reclamos),

      radioButtons(ns("nivel"), "Nivel geográfico:",
                   choices = c("Centro Comunal Zonal" = "ccz",
                               "Municipio" = "municipio"),
                   selected = "ccz"),

      hr(),
      div(style = "font-size:0.78rem; color:#94a3b8; line-height:1.45;",
          tags$b("Sobre este mapa"), tags$br(),
          "Son conteos de reclamos, no una medida directa de dónde hay más ",
          "problemas: también reflejan quién reclama.", tags$br(), tags$br(),
          tags$span(style = "color:#fbbf24;", mes_incompleto_txt)
      )
    ),

    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box(title = "Reclamos en el período", value = textOutput(ns("kpi_total")),
                showcase = icon("hashtag")),
      value_box(title = "Área con más reclamos", value = textOutput(ns("kpi_area")),
                showcase = icon("layer-group")),
      value_box(title = "% Finalizados", value = textOutput(ns("kpi_finalizados")),
                showcase = icon("check")),
      value_box(title = "Zona con más reclamos", value = textOutput(ns("kpi_zona")),
                showcase = icon("map-pin"))
    ),

    navset_card_underline(
      title = "Reclamos por zona",

      nav_panel("Mapa", icon = icon("map"),
                withSpinner(leafletOutput(ns("mapa"), height = "640px"), type = 4)),

      nav_panel("Ranking", icon = icon("chart-bar"),
                withSpinner(plotlyOutput(ns("ranking"), height = "640px"), type = 4)),

      nav_panel("Evolución mensual", icon = icon("chart-line"),
                withSpinner(plotlyOutput(ns("evolucion"), height = "640px"), type = 4)),

      nav_panel("Estacionalidad", icon = icon("calendar"),
                div(style = "font-size:0.82rem; color:#94a3b8; margin-bottom:10px;",
                    "Reclamos por mes calendario en cada año del período: sirve para ver ",
                    "si un área tiene estacionalidad (por ejemplo, arbolado después de ",
                    "temporales) o si un año se corta a mitad de camino."),
                withSpinner(plotlyOutput(ns("estacionalidad"), height = "560px"), type = 4)),

      nav_panel("Tabla", icon = icon("table"),
                div(style = "margin-bottom:10px;",
                    downloadButton(ns("bajar"), "Descargar CSV", class = "btn-sm")),
                withSpinner(DTOutput(ns("tabla")), type = 4))
    )
  )
}

mod_mapa_server <- function(id, ancho = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {

    reclamos_f <- reactive({
      req(input$areas, input$estados, input$periodo)
      reclamos_dt[anio >= input$periodo[1] & anio <= input$periodo[2] &
                   area %in% input$areas & estado %in% input$estados]
    })

    campo_zona <- reactive(input$nivel)

    titulo <- reactive({
      p <- input$periodo
      if (p[1] == p[2]) as.character(p[1]) else paste0(p[1], "–", p[2])
    })

    por_zona <- reactive({
      d <- reclamos_f()
      col <- campo_zona()
      d <- d[!is.na(get(col)), .(reclamos = .N), by = .(zona = as.character(get(col)))]
      d[order(-reclamos)]
    })

    # --- KPIs -----------------------------------------------------------------
    output$kpi_total <- renderText({
      fnum(nrow(reclamos_f()))
    })

    output$kpi_area <- renderText({
      d <- reclamos_f()
      if (nrow(d) == 0) return("—")
      tbl <- d[, .N, by = area][order(-N)]
      as.character(tbl$area[1])
    })

    output$kpi_finalizados <- renderText({
      d <- reclamos_f()
      if (nrow(d) == 0) return("—")
      paste0(fnum(100 * sum(d$estado == "Finalizado") / nrow(d), 1), "%")
    })

    output$kpi_zona <- renderText({
      d <- por_zona()
      if (nrow(d) == 0) return("—")
      etiqueta <- if (campo_zona() == "ccz") ccz_label else municipio_label
      z <- d$zona[1]
      if (is.na(etiqueta[z])) z else etiqueta[[z]]
    })

    # --- mapa -------------------------------------------------------------------
    output$mapa <- renderLeaflet({
      d <- por_zona()
      geo <- if (campo_zona() == "ccz") ccz_sf else municipios_sf
      campo_geo <- if (campo_zona() == "ccz") "ccz" else "municipio"

      geo <- merge(geo, d, by.x = campo_geo, by.y = "zona", all.x = TRUE)
      geo$reclamos[is.na(geo$reclamos)] <- 0

      cortes <- unique(quantile(geo$reclamos, probs = seq(0, 1, 0.125), na.rm = TRUE))
      pal <- if (length(cortes) > 2) {
        colorBin("YlOrRd", domain = geo$reclamos, bins = cortes, pretty = FALSE)
      } else {
        colorNumeric("YlOrRd", domain = geo$reclamos)
      }

      etiqueta_zona <- if (campo_zona() == "ccz") "CCZ" else "Municipio"
      nombre_zona <- geo[[campo_geo]]

      leaflet(geo) |>
        agregar_mapa_base() |>
        addPolygons(
          fillColor = ~pal(reclamos), fillOpacity = 0.75,
          weight = 0.8, color = "#64748b", opacity = 0.9,
          label = ~lapply(sprintf(
            "<b>%s %s</b><br>%s reclamos<br><i>%s</i>",
            etiqueta_zona, nombre_zona, format(reclamos, big.mark = "."), titulo()), htmltools::HTML),
          highlightOptions = highlightOptions(weight = 2.5, color = "#f8fafc",
                                              fillOpacity = 0.9, bringToFront = TRUE)
        ) |>
        addLegend(pal = pal, values = ~reclamos, title = paste("Reclamos", titulo()),
                  position = "bottomright", opacity = 0.85,
                  labFormat = labelFormat(big.mark = "."))
    })

    # --- ranking ------------------------------------------------------------------
    output$ranking <- renderPlotly({
      angosto <- es_angosto(ancho())
      etiqueta <- if (campo_zona() == "ccz") ccz_label else municipio_label
      d <- copy(por_zona())
      if (nrow(d) == 0) return(plotly_empty())
      d[, etiqueta := ifelse(is.na(etiqueta[zona]), zona, etiqueta[zona])]
      d <- head(d, top_n_barras(ancho()))
      d <- d[order(reclamos)]
      d[, etiqueta := factor(etiqueta, levels = etiqueta)]

      plot_ly(d, y = ~etiqueta, x = ~reclamos, type = "bar", orientation = "h",
              marker = list(color = "#3b82f6"),
              hovertemplate = "<b>%{y}</b><br>%{x:,.0f} reclamos<extra></extra>") |>
        layout(title = list(
                 text = if (angosto) paste("Top", nrow(d))
                        else paste(nrow(d), "zonas con más reclamos —", titulo()),
                 x = 0, xanchor = "left", xref = "paper"),
               xaxis = list(title = paste("Reclamos ·", titulo()),
                            separatethousands = TRUE,
                            gridcolor = grid_color_dark, zerolinecolor = grid_color_dark),
               yaxis = list(title = "", gridcolor = "transparent"),
               margin = list(l = margen_eje(ancho(), 200)),
               plot_bgcolor = plot_bg_color, paper_bgcolor = paper_bg_color,
               font = list(color = font_color_dark))
    })

    # --- evolución mensual ---------------------------------------------------------
    output$evolucion <- renderPlotly({
      d <- reclamos_f()[, .(reclamos = .N), by = .(mes = anio_mes, area = as.character(area))]
      if (nrow(d) == 0) return(plotly_empty())
      d <- d[order(mes)]

      p <- plot_ly()
      for (a in unique(d$area)) {
        sub <- d[area == a]
        p <- p |> add_trace(
          data = sub, x = ~mes, y = ~reclamos, type = "scatter", mode = "lines",
          name = a, line = list(color = color_area(a), width = 2),
          hovertemplate = paste0("<b>", a, "</b><br>%{x|%b %Y}<br>%{y:,.0f}<extra></extra>"))
      }
      p |> layout(title = "Reclamos por mes",
               xaxis = list(title = "", gridcolor = grid_color_dark),
               yaxis = list(title = "Reclamos", separatethousands = TRUE,
                            gridcolor = grid_color_dark),
               legend = list(orientation = "h", y = -0.2),
               hovermode = "x unified",
               plot_bgcolor = plot_bg_color, paper_bgcolor = paper_bg_color,
               font = list(color = font_color_dark))
    })

    # --- estacionalidad: mes calendario x año --------------------------------------
    output$estacionalidad <- renderPlotly({
      d <- reclamos_f()
      if (nrow(d) == 0) return(plotly_empty())
      agg <- d[, .(reclamos = .N),
              by = .(anio, mes_num = factor(data.table::month(fecha_ingreso),
                                            levels = as.character(1:12)))]
      # drop = FALSE: conserva las 12 columnas de mes aunque alguna quede en 0.
      m <- dcast(agg, anio ~ mes_num, value.var = "reclamos", fill = 0, drop = FALSE)
      anios_disp <- as.character(m$anio)
      m_mat <- as.matrix(m[, as.character(1:12), with = FALSE])

      plot_ly(x = meses_orden, y = anios_disp, z = m_mat, type = "heatmap",
              colors = "YlOrRd",
              hovertemplate = "%{y} — %{x}<br>%{z:,.0f} reclamos<extra></extra>") |>
        layout(xaxis = list(title = "", dtick = 1),
               yaxis = list(title = "", autorange = "reversed", dtick = 1),
               plot_bgcolor = plot_bg_color, paper_bgcolor = paper_bg_color,
               font = list(color = font_color_dark))
    })

    # --- tabla y descarga ------------------------------------------------------------
    tabla_datos <- reactive({
      col <- campo_zona()
      d <- reclamos_f()[!is.na(get(col)), .(reclamos = .N),
                        by = .(zona = as.character(get(col)), area = as.character(area))]
      w <- dcast(d, zona ~ area, value.var = "reclamos", fill = 0)
      w[, Total := rowSums(.SD), .SDcols = setdiff(names(w), "zona")]
      etiqueta <- if (col == "ccz") ccz_label else municipio_label
      w[, Zona := ifelse(is.na(etiqueta[zona]), zona, etiqueta[zona])]
      setcolorder(w, c("Zona", setdiff(names(w), c("Zona", "zona"))))
      w[, zona := NULL]
      w[order(-Total)]
    })

    output$tabla <- renderDT({
      datatable(tabla_datos(), rownames = FALSE, extensions = "Buttons",
                options = list(pageLength = 20, scrollX = TRUE,
                               language = list(url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/es-ES.json")))
    })

    output$bajar <- downloadHandler(
      filename = function() sprintf("reclamos_por_zona_%s.csv", gsub("–", "-", titulo())),
      content  = function(file) data.table::fwrite(tabla_datos(), file)
    )
  })
}
