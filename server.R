#' @title Logica del servidor del wrapper SPC basado en qicharts2
#'
#' @description
#' Coordina la carga de datos, la captura dinamica de parametros por analisis
#' y la ejecucion de wrappers sobre funciones de `qicharts2`.
#'
#' @details
#' Registra reactivos, validaciones, renderizados y acciones de usuario para
#' ejecutar los wrappers SPC, generar interpretaciones y exportar resultados.
#'
#' @param input Lista reactiva de entradas de Shiny.
#' @param output Lista reactiva de salidas de Shiny.
#' @param session Sesion activa de Shiny.
#'
#' @return Sin valor explicito. Se invoca por efectos laterales dentro de la
#'   aplicacion Shiny.
#' @keywords internal
NULL

#' @export
server <- function(input, output, session) {
  is_excel_file <- reactive({
    req(input$file)
    tolower(tools::file_ext(input$file$name)) %in% c("xls", "xlsx")
  })

  output$sheet_control <- renderUI({
    req(input$file)
    if (!is_excel_file()) {
      return(NULL)
    }

    if (!requireNamespace("readxl", quietly = TRUE)) {
      return(p("Para leer Excel instala el paquete ", code("readxl"), "."))
    }

    sheets <- readxl::excel_sheets(input$file$datapath)
    selectInput("sheet", "Hoja", choices = sheets, selected = sheets[[1]])
  })

  data_reactive <- reactive({
    req(input$file)
    read_input_data(
      path = input$file$datapath,
      header = input$header,
      sep = or_default(input$sep, ","),
      quote = or_default(input$quote, '"'),
      dec = or_default(input$dec, "."),
      sheet = if (is_excel_file()) input$sheet else NULL
    )
  })

  output$analysis_help <- renderUI({
    meta <- spc_analysis_catalog[[input$analysis_type]]
    req(meta)
    tagList(
      p(tags$strong(meta$label)),
      p(style = "color: #4b5563; margin-bottom: 0;", meta$description),
      p(style = "font-family: monospace; font-size: 0.85rem; color: #6b7280;", meta$function_name)
    )
  })

  output$parameter_panel <- renderUI({
    req(data_reactive())

    df <- data_reactive()
    cols <- names(df)
    choices <- column_choice_values(df)
    numeric_cols <- numeric_column_names(df)
    numeric_choices <- choices[unname(choices) %in% numeric_cols]
    any_choices <- c("Secuencia" = "_index", choices)
    optional_choices <- c("No usar" = "_none", choices)
    optional_numeric_choices <- c("No usar" = "_none", numeric_choices)
    default_numeric <- if (length(numeric_cols) > 0) numeric_cols[[1]] else cols[[1]]
    default_any <- cols[[1]]

    switch(
      input$analysis_type,
      qic = tagList(
        h4("Columnas"),
        selectInput("qic_y_col", "Y", choices = numeric_choices, selected = default_numeric),
        selectInput("qic_x_col", "X", choices = any_choices, selected = "_index"),
        selectInput("qic_n_col", "N / denominador", choices = optional_numeric_choices, selected = "_none"),
        selectInput("qic_facet_col", "Facet", choices = optional_choices, selected = "_none"),
        selectInput("qic_notes_col", "Notas", choices = optional_choices, selected = "_none"),
        tags$hr(),
        h4("Parametros"),
        selectInput("qic_chart_type", "Tipo de chart", choices = qic_chart_choices, selected = "i"),
        selectInput("qic_agg_fun", "Agregacion", choices = c("mean", "median", "sum", "sd"), selected = "mean"),
        selectInput("qic_method", "Metodo de runs", choices = c("anhoej", "bestbox", "cutbox"), selected = "anhoej"),
        numericInput("qic_multiply", "Multiplicar eje Y", value = 1, step = 1),
        numericInput("qic_freeze", "Freeze hasta punto", value = NA, min = 1, step = 1),
        numericInput("qic_target", "Target", value = NA, step = 0.1),
        numericInput("qic_cl", "Center line fija", value = NA, step = 0.1),
        checkboxInput("qic_show_95", "Mostrar limites 95%", FALSE),
        checkboxInput("qic_y_percent", "Formatear Y como porcentaje", FALSE),
        checkboxInput("qic_show_grid", "Mostrar grid", FALSE),
        checkboxInput("qic_y_neg", "Permitir valores negativos", TRUE),
        numericInput("qic_decimals", "Decimales", value = 1, min = 0, step = 1),
        numericInput("qic_point_size", "Tamano del punto", value = 1.5, min = 0.5, step = 0.5),
        numericInput("qic_x_angle", "Angulo etiquetas X", value = 0, min = 0, max = 90, step = 5),
        numericInput("qic_y_expand", "Expandir eje Y", value = NA, step = 0.1),
        textInput("qic_title", "Titulo", "qicharts2 SPC chart"),
        textInput("qic_subtitle", "Subtitulo", ""),
        textInput("qic_xlab", "Etiqueta eje X", "Subgroup"),
        textInput("qic_ylab", "Etiqueta eje Y", "Value")
      ),
      pareto = tagList(
        h4("Columnas"),
        selectInput("pareto_col", "Variable categorica", choices = choices, selected = default_any),
        tags$hr(),
        h4("Parametros"),
        checkboxInput("pareto_use_na", "Incluir NA", FALSE),
        numericInput("pareto_x_angle", "Angulo etiquetas X", value = 0, min = 0, max = 90, step = 5),
        textInput("pareto_title", "Titulo", "Pareto chart"),
        textInput("pareto_subtitle", "Subtitulo", ""),
        textInput("pareto_xlab", "Etiqueta eje X", ""),
        textInput("pareto_ylab", "Etiqueta eje Y", "")
      ),
      bchart = tagList(
        h4("Columnas"),
        selectInput("bchart_col", "Variable binaria", choices = choices, selected = default_any),
        tags$hr(),
        h4("Parametros"),
        numericInput("bchart_target", "Target", value = 0.05, step = 0.01),
        numericInput("bchart_or", "Odds ratio", value = 2, min = 0.1, step = 0.1),
        numericInput("bchart_limit", "Limite CUSUM", value = 3.5, min = 0.1, step = 0.1),
        textInput("bchart_title", "Titulo", "Bernoulli CUSUM"),
        textInput("bchart_xlab", "Etiqueta eje X", "Case #"),
        textInput("bchart_ylab", "Etiqueta eje Y", "CUSUM")
      )
    )
  })

  build_analysis_params <- reactive({
    req(data_reactive())
    df <- data_reactive()

    switch(
      input$analysis_type,
      qic = {
        validate(
          need(input$qic_y_col %in% names(df), "Selecciona una columna Y valida."),
          need(is.numeric(df[[input$qic_y_col]]), "La columna Y debe ser numerica."),
          need(identical(input$qic_x_col, "_index") || input$qic_x_col %in% names(df), "La columna X no existe."),
          need(identical(input$qic_n_col, "_none") || input$qic_n_col %in% names(df), "La columna N no existe."),
          need(identical(input$qic_n_col, "_none") || is.numeric(df[[input$qic_n_col]]), "La columna N debe ser numerica."),
          need(
            !(input$qic_chart_type %in% c("p", "pp", "u", "up") && identical(input$qic_n_col, "_none")),
            "Los charts p, pp, u y up requieren columna N."
          )
        )

        list(
          x_col = input$qic_x_col,
          y_col = input$qic_y_col,
          n_col = input$qic_n_col,
          facet_col = input$qic_facet_col,
          notes_col = input$qic_notes_col,
          chart_type = input$qic_chart_type,
          agg_fun = input$qic_agg_fun,
          method = input$qic_method,
          multiply = input$qic_multiply,
          freeze = if (is.na(input$qic_freeze)) NULL else as.integer(input$qic_freeze),
          target = if (is.na(input$qic_target)) NA_real_ else input$qic_target,
          cl = if (is.na(input$qic_cl)) NA_real_ else input$qic_cl,
          title = input$qic_title,
          subtitle = input$qic_subtitle,
          xlab = input$qic_xlab,
          ylab = input$qic_ylab,
          show_95 = isTRUE(input$qic_show_95),
          decimals = input$qic_decimals,
          point_size = input$qic_point_size,
          x_angle = input$qic_x_angle,
          y_expand = if (is.na(input$qic_y_expand)) NULL else input$qic_y_expand,
          y_neg = isTRUE(input$qic_y_neg),
          y_percent = isTRUE(input$qic_y_percent),
          show_grid = isTRUE(input$qic_show_grid)
        )
      },
      pareto = {
        validate(
          need(input$pareto_col %in% names(df), "Selecciona una variable valida.")
        )

        list(
          category_col = input$pareto_col,
          use_na = isTRUE(input$pareto_use_na),
          x_angle = input$pareto_x_angle,
          title = input$pareto_title,
          subtitle = input$pareto_subtitle,
          xlab = input$pareto_xlab,
          ylab = input$pareto_ylab
        )
      },
      bchart = {
        validate(
          need(input$bchart_col %in% names(df), "Selecciona una variable valida.")
        )

        list(
          outcome_col = input$bchart_col,
          target = input$bchart_target,
          odds_ratio = input$bchart_or,
          limit = input$bchart_limit,
          title = input$bchart_title,
          xlab = input$bchart_xlab,
          ylab = input$bchart_ylab
        )
      }
    )
  })

  analysis_result <- eventReactive(input$run_analysis, {
    req(data_reactive())
    params <- build_analysis_params()
    df <- data_reactive()

    captured <- NULL
    result <- NULL
    captured <- capture.output({
      result <- run_spc_analysis(input$analysis_type, df, params)
    })

    list(
      result = result,
      log = paste(captured, collapse = "\n")
    )
  })

  current_result <- reactive({
    req(analysis_result())
    analysis_result()$result
  })

  output$analysis_context <- renderUI({
    req(current_result())
    tags$div(
      style = "padding: 1rem 1rem 0 1rem; font-size: 0.95rem; color: #4b5563;",
      sprintf("%s: %s", current_result()$title, current_result()$subtitle)
    )
  })

  output$data_status <- renderUI({
    if (is.null(input$file)) {
      return(p("Carga un archivo CSV o Excel para habilitar el analisis."))
    }

    df <- data_reactive()
    tagList(
      p(sprintf("Filas: %s", nrow(df))),
      p(sprintf("Columnas: %s", ncol(df)))
    )
  })

  output$data_preview <- renderTable({
    req(data_reactive())
    utils::head(data_reactive(), 12)
  }, rownames = TRUE)

  output$summary_table <- renderTable({
    req(current_result())
    data_frame_from_named_list(current_result()$summary)
  }, rownames = FALSE)

  output$analysis_log <- renderText({
    req(analysis_result())
    analysis_result()$log
  })

  output$table_sections <- renderUI({
    req(current_result())
    tables <- current_result()$tables
    if (length(tables) < 1) {
      return(p("No hay tablas adicionales para este analisis."))
    }

    tagList(lapply(seq_along(tables), function(index) {
      table_name <- names(tables)[[index]]
      output_id <- sprintf("dynamic_table_%s", index)

      output[[output_id]] <- renderTable({
        current_result()$tables[[index]]
      }, rownames = FALSE)

      tagList(
        h4(table_name),
        tableOutput(output_id)
      )
    }))
  })

  output$analysis_plot <- renderImage({
    req(current_result())
    outfile <- tempfile(fileext = ".png")
    current_result()$build_plot(outfile)
    list(
      src = outfile,
      contentType = "image/png",
      alt = sprintf("Grafico %s", current_result()$title)
    )
  }, deleteFile = TRUE)

  observeEvent(input$copy_plot, {
    req(current_result())
    session$sendCustomMessage("copy-analysis-plot", list())
  })

  observeEvent(input$copy_plot_status, {
    status <- input$copy_plot_status
    req(is.list(status), !is.null(status$status), !is.null(status$detail))

    showNotification(
      status$detail,
      type = if (identical(status$status, "success")) "message" else "error"
    )
  })

  interpretation_result <- eventReactive(input$run_interpretation, {
    req(current_result())

    tryCatch(
      {
        plot_file <- tempfile(fileext = ".png")
        current_result()$build_plot(plot_file)

        withProgress(message = "Consultando OpenAI", value = 0.2, {
          incProgress(0.4)
          text <- openai_interpret_analysis(
            analysis_result = current_result(),
            plot_path = plot_file,
            language = "es",
            extra_instructions = or_default(input$interpretation_instructions, "")
          )
          incProgress(0.4)
          list(ok = TRUE, text = text)
        })
      },
      error = function(e) {
        list(ok = FALSE, text = conditionMessage(e))
      }
    )
  })

  output$interpretation_status <- renderText({
    req(current_result())

    if (input$run_interpretation < 1) {
      return("Sin consumir tokens. Pulsa 'Generar interpretacion' para consultar OpenAI.")
    }

    if (isTRUE(interpretation_result()$ok)) {
      "Interpretacion generada."
    } else {
      "No se pudo generar la interpretacion."
    }
  })

  output$interpretation_text <- renderUI({
    req(interpretation_result())

    card(
      card_body(
        tags$div(
          style = paste(
            "white-space: pre-wrap; line-height: 1.5;",
            if (isTRUE(interpretation_result()$ok)) "" else "color: #b91c1c;"
          ),
          interpretation_result()$text
        )
      )
    )
  })

  output$download_excel <- downloadHandler(
    filename = function() {
      req(current_result())
      label <- gsub("[^A-Za-z0-9_-]+", "-", paste(current_result()$analysis_id, current_result()$subtitle))
      sprintf("qicharts2-wrapper-%s-%s.xlsx", label, Sys.Date())
    },
    content = function(file) {
      req(current_result())
      plot_file <- tempfile(fileext = ".png")
      current_result()$build_plot(plot_file)

      interpretation_text <- NULL
      if (isTruthy(input$run_interpretation) && input$run_interpretation > 0) {
        interpretation_text <- interpretation_result()$text
      }

      write_analysis_export_workbook(
        path = file,
        analysis_result = current_result(),
        plot_path = plot_file,
        interpretation_text = interpretation_text
      )
    }
  )
}
