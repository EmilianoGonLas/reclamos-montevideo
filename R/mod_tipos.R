# ============================================================
# Módulo: Tipos de problema
# ============================================================
# Cada reclamo cae en un área > grupo > tipo de problema. Este módulo mira
# esa jerarquía completa (treemap) y el ranking de tipos específicos, que es
# donde está el detalle que un mapa por zona no puede mostrar.

mod_tipos_ui <- function(id) {
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

      hr(),
      div(style = "font-size:0.78rem; color:#94a3b8; line-height:1.45;",
          tags$b("Sobre estos datos"), tags$br(),
          "Área, grupo y tipo de problema son la clasificación que ",
          "carga el propio Sistema Único de Reclamos.", tags$br(), tags$br(),
          tags$span(style = "color:#fbbf24;", mes_incompleto_txt)
      )
    ),

    layout_columns(
      col_widths = c(4, 4, 4),
      value_box(title = "Reclamos en el período", value = textOutput(ns("kpi_total")),
                showcase = icon("hashtag")),
      value_box(title = "Tipos de problema distintos", value = textOutput(ns("kpi_tipos")),
                showcase = icon("list")),
      value_box(title = "Tipo más frecuente", value = textOutput(ns("kpi_top")),
                showcase = icon("star"))
    ),

    navset_card_underline(
      title = "Área · grupo · tipo de problema",

      nav_panel("Jerarquía", icon = icon("sitemap"),
                div(style = "font-size:0.82rem; color:#94a3b8; margin-bottom:10px;",
                    "Cada rectángulo es proporcional a la cantidad de reclamos. ",
                    "Un clic entra al área; otro clic en el centro vuelve atrás."),
                withSpinner(plotlyOutput(ns("treemap"), height = "620px"), type = 4)),

      nav_panel("Ranking de tipos", icon = icon("chart-bar"),
                withSpinner(plotlyOutput(ns("ranking"), height = "640px"), type = 4)),

      nav_panel("Tabla", icon = icon("table"),
                div(style = "margin-bottom:10px;",
                    downloadButton(ns("bajar"), "Descargar CSV", class = "btn-sm")),
                withSpinner(DTOutput(ns("tabla")), type = 4))
    )
  )
}

mod_tipos_server <- function(id, ancho = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {

    reclamos_f <- reactive({
      req(input$areas, input$estados, input$periodo)
      reclamos_dt[anio >= input$periodo[1] & anio <= input$periodo[2] &
                   area %in% input$areas & estado %in% input$estados]
    })

    titulo <- reactive({
      p <- input$periodo
      if (p[1] == p[2]) as.character(p[1]) else paste0(p[1], "–", p[2])
    })

    jerarquia <- reactive({
      reclamos_f()[, .(reclamos = .N),
                   by = .(area = as.character(area), grupo = as.character(grupo),
                          tipo_problema = as.character(tipo_problema))]
    })

    # --- KPIs -------------------------------------------------------------------
    output$kpi_total <- renderText(fnum(nrow(reclamos_f())))

    output$kpi_tipos <- renderText({
      fnum(length(unique(as.character(reclamos_f()$tipo_problema))))
    })

    output$kpi_top <- renderText({
      d <- reclamos_f()
      if (nrow(d) == 0) return("—")
      tbl <- d[, .N, by = tipo_problema][order(-N)]
      as.character(tbl$tipo_problema[1])
    })

    # --- treemap ------------------------------------------------------------------
    output$treemap <- renderPlotly({
      j <- jerarquia()
      if (nrow(j) == 0) return(plotly_empty())

      areas_u <- unique(j$area)
      grupos_u <- unique(paste(j$area, j$grupo, sep = " › "))

      ids <- c(areas_u, grupos_u, paste(j$area, j$grupo, j$tipo_problema, sep = " › "))
      labels <- c(areas_u, sub(".*› ", "", grupos_u), j$tipo_problema)
      parents <- c(rep("", length(areas_u)),
                   sub(" › [^›]+$", "", grupos_u),
                   paste(j$area, j$grupo, sep = " › "))
      values <- c(sapply(areas_u, function(a) sum(j$reclamos[j$area == a])),
                 sapply(grupos_u, function(g) {
                   partes <- strsplit(g, " › ")[[1]]
                   sum(j$reclamos[j$area == partes[1] & j$grupo == partes[2]])
                 }),
                 j$reclamos)
      colores <- c(color_area(areas_u), color_area(sub(" › .*", "", grupos_u)),
                  color_area(j$area))

      plot_ly(
        type = "treemap", ids = ids, labels = labels, parents = parents,
        values = values, branchvalues = "total",
        marker = list(colors = colores, line = list(color = "#0f172a", width = 2)),
        textfont = list(color = "#0f172a"),
        hovertemplate = "<b>%{label}</b><br>%{value:,.0f} reclamos<extra></extra>"
      ) |>
        layout(plot_bgcolor = plot_bg_color, paper_bgcolor = paper_bg_color,
               font = list(color = font_color_dark),
               margin = list(t = 10, l = 0, r = 0, b = 0))
    })

    # --- ranking de tipos -----------------------------------------------------------
    output$ranking <- renderPlotly({
      angosto <- es_angosto(ancho())
      d <- reclamos_f()[, .(reclamos = .N, area = area[1]),
                        by = .(tipo_problema = as.character(tipo_problema))]
      if (nrow(d) == 0) return(plotly_empty())
      d <- d[order(-reclamos)]
      d <- head(d, top_n_barras(ancho()))
      d <- d[order(reclamos)]
      d[, tipo_problema := factor(tipo_problema, levels = tipo_problema)]

      plot_ly(d, y = ~tipo_problema, x = ~reclamos, type = "bar", orientation = "h",
              marker = list(color = color_area(as.character(d$area))),
              hovertemplate = "<b>%{y}</b><br>%{x:,.0f} reclamos<extra></extra>") |>
        layout(title = list(
                 text = if (angosto) paste("Top", nrow(d))
                        else paste(nrow(d), "tipos de problema con más reclamos —", titulo()),
                 x = 0, xanchor = "left", xref = "paper"),
               xaxis = list(title = paste("Reclamos ·", titulo()),
                            separatethousands = TRUE,
                            gridcolor = grid_color_dark, zerolinecolor = grid_color_dark),
               yaxis = list(title = "", gridcolor = "transparent"),
               margin = list(l = margen_eje(ancho(), 340)),
               plot_bgcolor = plot_bg_color, paper_bgcolor = paper_bg_color,
               font = list(color = font_color_dark))
    })

    # --- tabla y descarga -------------------------------------------------------------
    tabla_datos <- reactive({
      d <- jerarquia()[order(-reclamos)]
      setnames(d, c("Área", "Grupo", "Tipo de problema", "Reclamos"))
      d
    })

    output$tabla <- renderDT({
      datatable(tabla_datos(), rownames = FALSE, extensions = "Buttons",
                options = list(pageLength = 25, scrollX = TRUE,
                               language = list(url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/es-ES.json")))
    })

    output$bajar <- downloadHandler(
      filename = function() sprintf("reclamos_por_tipo_%s.csv", gsub("–", "-", titulo())),
      content  = function(file) data.table::fwrite(tabla_datos(), file)
    )
  })
}
