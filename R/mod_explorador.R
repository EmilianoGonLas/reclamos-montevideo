# ============================================================
# Módulo: Explorador de reclamos
# ============================================================
# El nivel más fino: reclamo por reclamo, con mapa de puntos agrupados
# (cluster, porque un filtro amplio son cientos de miles de puntos) y tabla
# con paginado en el servidor, para no tener que mandar el detalle completo
# al navegador.

mod_explorador_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      title = "Filtros",
      width = 320,

      sliderInput(ns("periodo"), "Período (año de ingreso):",
                  min = min(anios_reclamos), max = max(anios_reclamos),
                  value = c(max(anios_reclamos), max(anios_reclamos)),
                  step = 1, sep = "", ticks = FALSE),

      selectizeInput(ns("area"), "Área:",
                     choices = c("Todas las áreas" = "", areas_reclamos),
                     selected = ""),

      selectizeInput(ns("tipo"), "Tipo de problema:",
                     choices = c("Todos los tipos" = ""), selected = ""),

      checkboxGroupInput(ns("estados"), "Estado del reclamo:",
                         choices = estados_reclamos, selected = estados_reclamos),

      hr(),
      div(style = "font-size:0.78rem; color:#94a3b8; line-height:1.45;",
          tags$b("Sobre el mapa"), tags$br(),
          "Con filtros amplios el mapa agrupa los puntos cercanos (el número ",
          "es la cantidad dentro del círculo); acercar el zoom los separa.", tags$br(), tags$br(),
          tags$span(style = "color:#fbbf24;", mes_incompleto_txt)
      )
    ),

    layout_columns(
      col_widths = c(4, 4, 4),
      value_box(title = "Reclamos filtrados", value = textOutput(ns("kpi_total")),
                showcase = icon("hashtag")),
      value_box(title = "Finalizados", value = textOutput(ns("kpi_finalizados")),
                showcase = icon("check")),
      value_box(title = "Mediana de días (finalizados)", value = textOutput(ns("kpi_mediana")),
                showcase = icon("stopwatch"))
    ),

    navset_card_underline(
      title = "Reclamos individuales",

      nav_panel("Mapa de puntos", icon = icon("map-location-dot"),
                withSpinner(leafletOutput(ns("mapa"), height = "640px"), type = 4)),

      nav_panel("Tabla", icon = icon("table"),
                div(style = "margin-bottom:10px;",
                    downloadButton(ns("bajar"), "Descargar CSV (hasta 200.000 filas)", class = "btn-sm")),
                withSpinner(DTOutput(ns("tabla")), type = 4))
    )
  )
}

mod_explorador_server <- function(id, ancho = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {

    # El selector de tipo se limita a lo que existe dentro del área elegida.
    observeEvent(input$area, {
      choices <- if (input$area == "") {
        sort(unique(as.character(reclamos_dt$tipo_problema)))
      } else {
        tipos_por_area[.(input$area)]$tipo_problema
      }
      updateSelectizeInput(session, "tipo",
                           choices = c("Todos los tipos" = "", choices), selected = "")
    }, ignoreNULL = FALSE)

    reclamos_f <- reactive({
      req(input$estados, input$periodo)
      d <- reclamos_dt[anio >= input$periodo[1] & anio <= input$periodo[2] &
                        estado %in% input$estados]
      if (!is.null(input$area) && input$area != "") d <- d[area == input$area]
      if (!is.null(input$tipo) && input$tipo != "") d <- d[tipo_problema == input$tipo]
      d
    })

    # --- KPIs -------------------------------------------------------------------
    output$kpi_total <- renderText(fnum(nrow(reclamos_f())))

    output$kpi_finalizados <- renderText({
      d <- reclamos_f()
      if (nrow(d) == 0) return("—")
      paste0(fnum(100 * sum(d$estado == "Finalizado") / nrow(d), 1), "%")
    })

    output$kpi_mediana <- renderText({
      d <- reclamos_f()[estado == "Finalizado" & !is.na(dias_resolucion)]
      if (nrow(d) == 0) return("—")
      fnum(stats::median(d$dias_resolucion), 0)
    })

    # --- mapa de puntos (con cluster) --------------------------------------------
    TOPE_PUNTOS <- 60000

    output$mapa <- renderLeaflet({
      d <- reclamos_f()
      n_total <- nrow(d)
      if (n_total == 0) {
        return(leaflet() |> agregar_mapa_base() |>
                 setView(lng = -56.18, lat = -34.85, zoom = 11))
      }
      if (n_total > TOPE_PUNTOS) d <- d[sample(.N, TOPE_PUNTOS)]

      etiquetas <- sprintf(
        "<b>%s</b><br>%s<br>%s<br>%s<br>%s",
        as.character(d$tipo_problema), as.character(d$area),
        as.character(d$estado), format(d$fecha_ingreso, "%d/%m/%Y"),
        paste("CCZ", ifelse(is.na(d$ccz), "—", as.character(d$ccz))))

      mapa <- leaflet(d) |>
        agregar_mapa_base() |>
        addCircleMarkers(
          lng = ~lon, lat = ~lat, radius = 5, stroke = FALSE,
          fillOpacity = 0.75, fillColor = ~color_area(as.character(area)),
          label = ~lapply(etiquetas, htmltools::HTML),
          clusterOptions = markerClusterOptions(maxClusterRadius = 45)
        )

      if (n_total > TOPE_PUNTOS) {
        mapa <- mapa |> addControl(
          sprintf("Muestra aleatoria de %s de %s reclamos filtrados",
                  fnum(TOPE_PUNTOS), fnum(n_total)),
          position = "topright")
      }
      mapa
    })

    # --- tabla y descarga ---------------------------------------------------------
    tabla_datos <- reactive({
      d <- reclamos_f()[, .(
        Número = numero_reclamo,
        Fecha = fecha_ingreso,
        Área = as.character(area),
        Grupo = as.character(grupo),
        Tipo = as.character(tipo_problema),
        Estado = as.character(estado),
        Días = dias_resolucion,
        CCZ = as.character(ccz),
        Municipio = as.character(municipio)
      )]
      d[order(-Fecha)]
    })

    output$tabla <- renderDT({
      datatable(tabla_datos(), rownames = FALSE, extensions = "Buttons",
                filter = "top", server = TRUE,
                options = list(pageLength = 25, scrollX = TRUE,
                               language = list(url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/es-ES.json")))
    })

    output$bajar <- downloadHandler(
      filename = function() "reclamos_filtrados.csv",
      content  = function(file) data.table::fwrite(head(tabla_datos(), 200000), file)
    )
  })
}
