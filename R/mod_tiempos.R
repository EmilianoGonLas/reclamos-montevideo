# ============================================================
# Módulo: Tiempos de resolución
# ============================================================
# Sólo tiene sentido sobre reclamos "Finalizado": para el resto,
# fecha_desde_en_estado es la última transición registrada hoy, no un
# cierre. Y como la fuente se sobrescribe cada mes con la situación
# vigente, esto es una foto actual, no el historial completo de cada
# reclamo (si se reabrió, no queda rastro).

mod_tiempos_ui <- function(id) {
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

      sliderInput(ns("recorte"), "Recortar outliers (percentil superior):",
                  min = 90, max = 100, value = 99, step = 1, post = "%"),

      hr(),
      div(style = "font-size:0.78rem; color:#94a3b8; line-height:1.45;",
          tags$b("Sobre estos datos"), tags$br(),
          "Sólo se cuentan reclamos ", tags$b("Finalizados"), ". El tiempo es ",
          "la última transición de estado que quedó registrada hoy: si un ",
          "reclamo se reabrió, no se ve. Un puñado de reclamos muy viejos ",
          "estira mucho la cola; el recorte de percentil es sólo para que el ",
          "gráfico no quede aplastado, no cambia la mediana.", tags$br(), tags$br(),
          tags$span(style = "color:#fbbf24;", mes_incompleto_txt)
      )
    ),

    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box(title = "Reclamos finalizados", value = textOutput(ns("kpi_total")),
                showcase = icon("check-double")),
      value_box(title = "Mediana de días", value = textOutput(ns("kpi_mediana")),
                showcase = icon("stopwatch")),
      value_box(title = "Resueltos en ≤ 7 días", value = textOutput(ns("kpi_rapidos")),
                showcase = icon("bolt")),
      value_box(title = "Resueltos en > 90 días", value = textOutput(ns("kpi_lentos")),
                showcase = icon("hourglass-half"))
    ),

    navset_card_underline(
      title = "Tiempos de resolución (sólo reclamos finalizados)",

      nav_panel("Por área", icon = icon("chart-simple"),
                withSpinner(plotlyOutput(ns("caja_area"), height = "620px"), type = 4)),

      nav_panel("Por tipo de problema", icon = icon("list-ol"),
                div(style = "font-size:0.82rem; color:#94a3b8; margin-bottom:10px;",
                    "Los 20 tipos de problema más rápidos y los 20 más lentos ",
                    "en mediana de días, con al menos 30 reclamos finalizados en el período."),
                withSpinner(plotlyOutput(ns("ranking_tipo"), height = "700px"), type = 4)),

      nav_panel("Evolución", icon = icon("chart-line"),
                withSpinner(plotlyOutput(ns("evolucion"), height = "620px"), type = 4)),

      nav_panel("Tabla", icon = icon("table"),
                div(style = "margin-bottom:10px;",
                    downloadButton(ns("bajar"), "Descargar CSV", class = "btn-sm")),
                withSpinner(DTOutput(ns("tabla")), type = 4))
    )
  )
}

mod_tiempos_server <- function(id, ancho = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {

    finalizados_f <- reactive({
      req(input$areas, input$periodo)
      reclamos_dt[anio >= input$periodo[1] & anio <= input$periodo[2] &
                   area %in% input$areas & estado == "Finalizado" &
                   !is.na(dias_resolucion)]
    })

    titulo <- reactive({
      p <- input$periodo
      if (p[1] == p[2]) as.character(p[1]) else paste0(p[1], "–", p[2])
    })

    # --- KPIs -----------------------------------------------------------------
    output$kpi_total <- renderText(fnum(nrow(finalizados_f())))

    output$kpi_mediana <- renderText({
      d <- finalizados_f()
      if (nrow(d) == 0) return("—")
      fnum(stats::median(d$dias_resolucion), 0)
    })

    output$kpi_rapidos <- renderText({
      d <- finalizados_f()
      if (nrow(d) == 0) return("—")
      paste0(fnum(100 * sum(d$dias_resolucion <= 7) / nrow(d), 1), "%")
    })

    output$kpi_lentos <- renderText({
      d <- finalizados_f()
      if (nrow(d) == 0) return("—")
      paste0(fnum(100 * sum(d$dias_resolucion > 90) / nrow(d), 1), "%")
    })

    # --- caja por área ----------------------------------------------------------
    output$caja_area <- renderPlotly({
      d0 <- finalizados_f()
      if (nrow(d0) == 0) return(plotly_empty())
      tope <- stats::quantile(d0$dias_resolucion, input$recorte / 100, na.rm = TRUE)
      # Selección de columnas (no filtro por referencia): crea una copia nueva,
      # así el factor que se arma abajo no pisa el data.table cacheado por el
      # reactive (otros outputs también leen finalizados_f()).
      d <- d0[dias_resolucion <= tope, .(area = as.character(area), dias_resolucion)]

      orden <- d[, .(mediana = stats::median(dias_resolucion)), by = area][order(mediana)]
      d[, area := factor(area, levels = orden$area)]

      plot_ly(d, y = ~area, x = ~dias_resolucion, type = "box", orientation = "h",
              color = ~area, colors = colores_area,
              line = list(color = "#94a3b8"),
              hovertemplate = "%{x:,.0f} días<extra></extra>") |>
        layout(title = paste("Días hasta el cierre, por área —", titulo()),
               xaxis = list(title = "Días hábiles/corridos hasta el cierre",
                            gridcolor = grid_color_dark, zerolinecolor = grid_color_dark),
               yaxis = list(title = "", gridcolor = "transparent"),
               margin = list(l = margen_eje(ancho(), 200)),
               showlegend = FALSE,
               plot_bgcolor = plot_bg_color, paper_bgcolor = paper_bg_color,
               font = list(color = font_color_dark))
    })

    # --- ranking por tipo (rápidos y lentos) --------------------------------------
    output$ranking_tipo <- renderPlotly({
      d <- finalizados_f()[, .(mediana = stats::median(dias_resolucion), n = .N, area = area[1]),
                           by = .(tipo_problema = as.character(tipo_problema))]
      d <- d[n >= 30]
      if (nrow(d) == 0) return(plotly_empty())

      n_lado <- if (es_angosto(ancho())) 8L else 20L
      rapidos <- head(d[order(mediana)], n_lado)
      lentos  <- head(d[order(-mediana)], n_lado)
      rapidos[, grupo := "Más rápidos"]
      lentos[, grupo := "Más lentos"]
      dd <- rbind(rapidos, lentos)
      dd[, orden := ifelse(grupo == "Más rápidos", mediana, -mediana)]
      dd <- dd[order(grupo, -orden)]
      dd[, tipo_problema := factor(tipo_problema, levels = rev(unique(tipo_problema)))]

      plot_ly(dd, y = ~tipo_problema, x = ~mediana, type = "bar", orientation = "h",
              color = ~grupo, colors = c("Más rápidos" = "#34d399", "Más lentos" = "#ef4444"),
              hovertemplate = "<b>%{y}</b><br>Mediana: %{x:,.0f} días<extra></extra>") |>
        layout(title = paste("Mediana de días hasta el cierre, por tipo de problema (mín. 30 casos) —", titulo()),
               xaxis = list(title = "Mediana de días", gridcolor = grid_color_dark),
               yaxis = list(title = "", gridcolor = "transparent"),
               margin = list(l = margen_eje(ancho(), 320)),
               legend = list(orientation = "h", y = -0.08),
               plot_bgcolor = plot_bg_color, paper_bgcolor = paper_bg_color,
               font = list(color = font_color_dark))
    })

    # --- evolución mensual de la mediana -----------------------------------------
    output$evolucion <- renderPlotly({
      d <- finalizados_f()[, .(mediana = stats::median(dias_resolucion)),
                           by = .(mes = anio_mes, area = as.character(area))]
      if (nrow(d) == 0) return(plotly_empty())
      d <- d[order(mes)]

      p <- plot_ly()
      for (a in unique(d$area)) {
        sub <- d[area == a]
        p <- p |> add_trace(
          data = sub, x = ~mes, y = ~mediana, type = "scatter", mode = "lines",
          name = a, line = list(color = color_area(a), width = 2),
          hovertemplate = paste0("<b>", a, "</b><br>%{x|%b %Y}<br>Mediana: %{y:,.0f} días<extra></extra>"))
      }
      p |> layout(title = "Mediana mensual de días hasta el cierre",
               xaxis = list(title = "", gridcolor = grid_color_dark),
               yaxis = list(title = "Días", gridcolor = grid_color_dark),
               legend = list(orientation = "h", y = -0.2),
               hovermode = "x unified",
               plot_bgcolor = plot_bg_color, paper_bgcolor = paper_bg_color,
               font = list(color = font_color_dark))
    })

    # --- tabla y descarga ---------------------------------------------------------
    tabla_datos <- reactive({
      d <- finalizados_f()[, .(reclamos = .N,
                              mediana_dias = stats::median(dias_resolucion),
                              p90_dias = stats::quantile(dias_resolucion, 0.9)),
                           by = .(Área = as.character(area))]
      d[order(mediana_dias)]
    })

    output$tabla <- renderDT({
      datatable(tabla_datos(), rownames = FALSE, extensions = "Buttons",
                options = list(pageLength = 20, scrollX = TRUE,
                               language = list(url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/es-ES.json"))) |>
        formatRound(c("mediana_dias", "p90_dias"), 1)
    })

    output$bajar <- downloadHandler(
      filename = function() sprintf("tiempos_resolucion_%s.csv", gsub("–", "-", titulo())),
      content  = function(file) data.table::fwrite(tabla_datos(), file)
    )
  })
}
