#' @title Interfaz de usuario del wrapper SPC basado en qicharts2
#'
#' @description
#' Interfaz Shiny para seleccionar analisis soportados por `qicharts2`,
#' configurar parametros por funcion y revisar salidas tabulares y graficas.
#'
#' @keywords internal
NULL

library(shiny)
library(bslib)

#' @export
ui <- page_sidebar(
  title = "Wrapper SPC con qicharts2",
  tags$head(
    tags$style(HTML("
      #analysis_plot img {
        display: block;
        width: 100%;
        max-width: 100%;
        height: auto;
      }
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('copy-analysis-plot', async function(message) {
        const container = document.getElementById('analysis_plot');
        const img = container ? container.querySelector('img') : null;

        if (!img || !img.src) {
          Shiny.setInputValue('copy_plot_status', {
            status: 'missing',
            detail: 'No hay grafico disponible para copiar.',
            nonce: Date.now()
          }, { priority: 'event' });
          return;
        }

        if (!navigator.clipboard || typeof ClipboardItem === 'undefined') {
          Shiny.setInputValue('copy_plot_status', {
            status: 'unsupported',
            detail: 'El navegador no permite copiar imagenes al portapapeles.',
            nonce: Date.now()
          }, { priority: 'event' });
          return;
        }

        try {
          const response = await fetch(img.src);
          const blob = await response.blob();
          await navigator.clipboard.write([
            new ClipboardItem({ [blob.type || 'image/png']: blob })
          ]);
          Shiny.setInputValue('copy_plot_status', {
            status: 'success',
            detail: 'Grafico copiado al portapapeles.',
            nonce: Date.now()
          }, { priority: 'event' });
        } catch (error) {
          Shiny.setInputValue('copy_plot_status', {
            status: 'error',
            detail: error && error.message ? error.message : 'No se pudo copiar el grafico.',
            nonce: Date.now()
          }, { priority: 'event' });
        }
      });
    "))
  ),
  sidebar = sidebar(
    width = 360,
    h4("Analisis"),
    selectInput(
      "analysis_type",
      "Wrapper de qicharts2",
      choices = spc_analysis_choices(),
      selected = "qic"
    ),
    uiOutput("analysis_help"),
    tags$hr(),
    h4("Datos"),
    fileInput("file", "Archivo", accept = c(".csv", ".txt", ".xls", ".xlsx")),
    checkboxInput("header", "La primera fila es encabezado", TRUE),
    uiOutput("sheet_control"),
    conditionalPanel(
      condition = "input.file && !/\\.(xls|xlsx)$/i.test(input.file.name || '')",
      selectInput(
        "sep",
        "Separador",
        choices = c("Coma" = ",", "Punto y coma" = ";", "Tab" = "\t"),
        selected = ","
      )
    ),
    conditionalPanel(
      condition = "input.file && !/\\.(xls|xlsx)$/i.test(input.file.name || '')",
      selectInput(
        "dec",
        "Separador decimal",
        choices = c("Punto" = ".", "Coma" = ","),
        selected = "."
      )
    ),
    conditionalPanel(
      condition = "input.file && !/\\.(xls|xlsx)$/i.test(input.file.name || '')",
      selectInput(
        "quote",
        "Comillas",
        choices = c('Doble comilla' = '"', "Simple comilla" = "'", "Ninguna" = ""),
        selected = '"'
      )
    ),
    tags$hr(),
    uiOutput("parameter_panel"),
    actionButton("run_analysis", "Ejecutar analisis", class = "btn-primary")
  ),
  card(
    full_screen = TRUE,
    uiOutput("analysis_context"),
    tags$div(
      style = "padding: 0 1rem 1rem 1rem;",
      downloadButton("download_excel", "Exportar resultados Excel")
    ),
    navset_card_tab(
      nav_panel(
        "Vista previa",
        br(),
        uiOutput("data_status"),
        tableOutput("data_preview")
      ),
      nav_panel(
        "Resultados",
        br(),
        h4("Resumen"),
        tableOutput("summary_table"),
        h4("Traza de ejecucion"),
        verbatimTextOutput("analysis_log"),
        uiOutput("table_sections")
      ),
      nav_panel(
        "Grafico",
        br(),
        tags$div(
          style = "margin-bottom: 1rem;",
          actionButton("copy_plot", "Copiar grafico al portapapeles")
        ),
        imageOutput("analysis_plot", width = "100%", height = "auto")
      ),
      nav_panel(
        "Interpretacion",
        br(),
        p("La interpretacion con OpenAI solo se ejecuta cuando pulses el boton."),
        textAreaInput(
          "interpretation_instructions",
          "Instrucciones adicionales",
          placeholder = "Ejemplo: enfocate en estabilidad del proceso, senales especiales y acciones recomendadas.",
          rows = 4,
          width = "100%"
        ),
        tags$div(
          style = "margin-bottom: 1rem;",
          actionButton("run_interpretation", "Generar interpretacion", class = "btn-primary")
        ),
        verbatimTextOutput("interpretation_status"),
        uiOutput("interpretation_text")
      ),
      nav_panel(
        "Ayuda",
        br(),
        p("La app es un wrapper Shiny alrededor de funciones de la libreria ", code("qicharts2"), "."),
        tags$ul(
          tags$li(code("qicharts2::qic"), " para run charts y cartas de control"),
          tags$li(code("qicharts2::paretochart"), " para Pareto"),
          tags$li(code("qicharts2::bchart"), " para Bernoulli CUSUM")
        ),
        p("Si el paquete no esta instalado, en R usa: ", code("install.packages('qicharts2')")),
        p("Para interpretacion con OpenAI configura ", code("OPENAI_API_KEY"), " y opcionalmente ", code("OPENAI_MODEL"), ".")
      )
    )
  )
)
