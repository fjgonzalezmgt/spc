#' @title Utilidades globales para el wrapper SPC basado en qicharts2
#'
#' @description
#' Helpers compartidos para leer datos, describir analisis soportados y
#' ejecutar wrappers alrededor de funciones de la libreria `qicharts2`.
#'
#' @keywords internal
NULL

library(shiny)

if (file.exists("openai_helpers.R")) {
  source("openai_helpers.R", local = TRUE)
}

#' Devuelve un valor por defecto cuando la entrada esta vacia
#'
#' @param x Valor opcional.
#' @param default Valor de respaldo que se devolvera cuando `x` sea `NULL`
#'   o una cadena vacia.
#'
#' @return `x` si tiene contenido; en caso contrario `default`.
or_default <- function(x, default) {
  if (is.null(x) || identical(x, "")) {
    default
  } else {
    x
  }
}

#' Verifica que qicharts2 este disponible
#'
#' @return `NULL` de forma invisible. Lanza un error si el paquete
#'   `qicharts2` no esta instalado.
require_qicharts2 <- function() {
  if (!requireNamespace("qicharts2", quietly = TRUE)) {
    stop(
      paste(
        "El paquete 'qicharts2' no esta instalado.",
        "Instalalo con install.packages('qicharts2')."
      ),
      call. = FALSE
    )
  }
}

#' Lee archivos delimitados con configuracion explicita
#'
#' @param path Ruta al archivo de texto.
#' @param header Indicador logico de encabezado.
#' @param sep Separador de columnas.
#' @param quote Caracter de comillas.
#' @param dec Separador decimal.
#'
#' @return Un `data.frame` con `stringsAsFactors = FALSE`.
read_delimited_data <- function(path, header, sep, quote, dec) {
  utils::read.table(
    file = path,
    header = header,
    sep = sep,
    quote = quote,
    dec = dec,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

#' Lee datos de entrada desde CSV, TXT o Excel
#'
#' @param path Ruta al archivo de origen.
#' @param header Indicador logico de encabezado para archivos delimitados.
#' @param sep Separador de columnas para archivos delimitados.
#' @param quote Caracter de comillas para archivos delimitados.
#' @param dec Separador decimal para archivos delimitados.
#' @param sheet Hoja de Excel a leer cuando el archivo es `.xls` o `.xlsx`.
#'
#' @return Un `data.frame` con los datos importados.
#' @export
read_input_data <- function(path, header, sep, quote, dec, sheet = NULL) {
  ext <- tolower(tools::file_ext(path))

  if (ext %in% c("xls", "xlsx")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop(
        paste(
          "El paquete 'readxl' no esta instalado.",
          "Instalalo con install.packages('readxl')."
        ),
        call. = FALSE
      )
    }

    return(as.data.frame(readxl::read_excel(path = path, sheet = sheet)))
  }

  read_delimited_data(
    path = path,
    header = header,
    sep = sep,
    quote = quote,
    dec = dec
  )
}

spc_analysis_catalog <- list(
  qic = list(
    label = "SPC chart",
    function_name = "qicharts2::qic",
    description = "Run charts y cartas de control Shewhart: I, MR, Xbar, S, T, C, U, P, G y variantes."
  ),
  pareto = list(
    label = "Pareto chart",
    function_name = "qicharts2::paretochart",
    description = "Diagrama de Pareto para variables categoricas."
  ),
  bchart = list(
    label = "Bernoulli CUSUM",
    function_name = "qicharts2::bchart",
    description = "CUSUM Bernoulli para eventos binarios raros."
  )
)

#' Construye las opciones visibles de analisis SPC
#'
#' @return Un vector nombrado para usar en `selectInput()`, donde los valores
#'   son los ids internos de analisis.
spc_analysis_choices <- function() {
  stats::setNames(names(spc_analysis_catalog), vapply(
    spc_analysis_catalog,
    function(x) sprintf("%s (%s)", x$label, x$function_name),
    character(1)
  ))
}

qic_chart_choices <- c(
  "Run chart" = "run",
  "I chart" = "i",
  "Individuals p chart" = "ip",
  "Moving range" = "mr",
  "Xbar" = "xbar",
  "S chart" = "s",
  "T chart" = "t",
  "P chart" = "p",
  "P prime" = "pp",
  "C chart" = "c",
  "U chart" = "u",
  "U prime" = "up",
  "G chart" = "g"
)

#' Identifica columnas numericas
#'
#' @param df `data.frame` de entrada.
#'
#' @return Un vector de caracteres con nombres de columnas numericas.
numeric_column_names <- function(df) {
  names(df)[vapply(df, is.numeric, logical(1))]
}

#' Construye etiquetas de columnas con su clase
#'
#' @param df `data.frame` de entrada.
#'
#' @return Un vector nombrado donde cada etiqueta combina nombre de columna y
#'   clase detectada.
column_choice_values <- function(df) {
  cols <- names(df)
  labels <- vapply(
    cols,
    function(col) sprintf("%s (%s)", col, class(df[[col]])[[1]]),
    character(1)
  )
  stats::setNames(cols, labels)
}

#' Convierte una lista nombrada en tabla de metricas
#'
#' @param x Lista nombrada con valores escalares o vectores cortos.
#'
#' @return Un `data.frame` con columnas `Metrica` y `Valor`.
data_frame_from_named_list <- function(x) {
  if (length(x) < 1) {
    return(data.frame())
  }

  data.frame(
    Metrica = names(x),
    Valor = vapply(x, function(value) {
      if (length(value) > 1) {
        paste(format(value), collapse = ", ")
      } else {
        as.character(value)
      }
    }, character(1)),
    row.names = NULL,
    check.names = FALSE
  )
}

#' Normaliza nombres de hojas de Excel
#'
#' @param x Nombre propuesto para una hoja.
#'
#' @return Una cadena compatible con las restricciones de Excel.
sanitize_sheet_name <- function(x) {
  cleaned <- gsub("[\\\\/:*?\\[\\]]", "-", x)
  substr(cleaned, 1, 31)
}

#' Sustituye marcadores vacios por una etiqueta legible
#'
#' @param value Valor a evaluar.
#' @param missing_label Texto a usar cuando el valor es vacio o representa
#'   ausencia.
#'
#' @return El valor original o `missing_label`.
non_empty_value <- function(value, missing_label = "No usado") {
  if (is.null(value) || identical(value, "") || identical(value, "_none")) {
    missing_label
  } else {
    value
  }
}

#' Exporta un objeto ggplot a archivo
#'
#' @param plot_obj Objeto grafico compatible con `ggplot2::ggsave()`.
#' @param path Ruta del archivo de salida.
#' @param width Ancho en pulgadas.
#' @param height Alto en pulgadas.
#' @param dpi Resolucion de salida.
#'
#' @return La ruta exportada, de forma invisible.
build_plot_export <- function(plot_obj, path, width = 12, height = 7, dpi = 180) {
  ggplot2::ggsave(
    filename = path,
    plot = plot_obj,
    width = width,
    height = height,
    dpi = dpi,
    units = "in"
  )
  invisible(path)
}

#' Evalua una expresion con un dispositivo grafico temporal
#'
#' @param code Expresion a evaluar.
#'
#' @return El resultado de `code`.
suppress_default_plot_file <- function(code) {
  temp_plot <- tempfile(fileext = ".png")
  grDevices::png(filename = temp_plot, width = 480, height = 480)
  on.exit({
    if (grDevices::dev.cur() > 1) {
      grDevices::dev.off()
    }
    unlink(temp_plot)
  }, add = TRUE)

  force(code)
}

#' Intenta eliminar el archivo lateral `Rplots.pdf`
#'
#' @details
#' Algunos flujos no interactivos de R pueden materializar `Rplots.pdf` en el
#' directorio de trabajo. Esta utilidad intenta eliminarlo de forma segura.
#'
#' @return `TRUE` si el archivo ya no existe al terminar; `FALSE` en caso
#'   contrario.
cleanup_rplots_pdf <- function() {
  if (!file.exists("Rplots.pdf")) {
    return(invisible(FALSE))
  }

  try(closeAllConnections(), silent = TRUE)

  removed <- suppressWarnings(file.remove("Rplots.pdf"))
  if (!isTRUE(removed) && file.exists("Rplots.pdf")) {
    suppressWarnings(unlink("Rplots.pdf", force = TRUE))
  }

  invisible(!file.exists("Rplots.pdf"))
}

#' Construye la tabla base para un diagrama de Pareto
#'
#' @param x Vector categorico.
#' @param use_na Indicador logico para incluir `NA`.
#'
#' @return Un `data.frame` con frecuencia, porcentaje y porcentaje acumulado.
build_pareto_table <- function(x, use_na = FALSE) {
  counts <- sort(table(x, useNA = if (isTRUE(use_na)) "ifany" else "no"), decreasing = TRUE)
  df <- data.frame(
    Categoria = names(counts),
    Frecuencia = as.integer(counts),
    row.names = NULL,
    check.names = FALSE
  )
  df$Porcentaje <- df$Frecuencia / sum(df$Frecuencia)
  df$`Porcentaje acumulado` <- cumsum(df$Porcentaje)
  df
}

#' Convierte una columna binaria a logica
#'
#' @param x Vector logico o numerico con valores `0/1`.
#'
#' @return Un vector logico.
coerce_binary_vector <- function(x) {
  if (is.logical(x)) {
    return(x)
  }

  if (is.numeric(x)) {
    allowed <- stats::na.omit(unique(x))
    if (all(allowed %in% c(0, 1))) {
      return(as.logical(x))
    }
  }

  stop(
    "La columna binaria debe ser logica o numerica con valores 0/1.",
    call. = FALSE
  )
}

#' Ejecuta un analisis SPC basado en `qicharts2::qic`
#'
#' @param df `data.frame` con los datos de entrada.
#' @param params Lista de parametros construida desde la interfaz.
#'
#' @return Una lista con metadatos, tablas, grafico y helper de exportacion.
run_qic_analysis <- function(df, params) {
  require_qicharts2()

  facet_formula <- if (identical(params$facet_col, "_none")) {
    NULL
  } else {
    stats::as.formula(paste("~", params$facet_col))
  }

  x_value <- if (identical(params$x_col, "_index")) {
    df[[params$y_col]]
  } else {
    df[[params$x_col]]
  }

  y_value <- if (identical(params$x_col, "_index")) {
    NULL
  } else {
    df[[params$y_col]]
  }

  n_value <- if (identical(params$n_col, "_none")) NULL else df[[params$n_col]]
  notes_value <- if (identical(params$notes_col, "_none")) NULL else df[[params$notes_col]]

  plot_obj <- suppress_default_plot_file(qicharts2::qic(
    x = x_value,
    y = y_value,
    n = n_value,
    data = df,
    facets = facet_formula,
    notes = notes_value,
    chart = params$chart_type,
    agg.fun = params$agg_fun,
    method = params$method,
    multiply = params$multiply,
    freeze = params$freeze,
    target = params$target,
    cl = params$cl,
    title = params$title,
    ylab = params$ylab,
    xlab = params$xlab,
    subtitle = params$subtitle,
    show.95 = params$show_95,
    decimals = params$decimals,
    point.size = params$point_size,
    x.angle = params$x_angle,
    y.expand = params$y_expand,
    y.neg = params$y_neg,
    y.percent = params$y_percent,
    show.grid = params$show_grid
  ))

  summary_df <- summary(plot_obj)
  detail_df <- qicharts2::qic(
    x = x_value,
    y = y_value,
    n = n_value,
    data = df,
    facets = facet_formula,
    notes = notes_value,
    chart = params$chart_type,
    agg.fun = params$agg_fun,
    method = params$method,
    multiply = params$multiply,
    freeze = params$freeze,
    target = params$target,
    cl = params$cl,
    title = params$title,
    ylab = params$ylab,
    xlab = params$xlab,
    subtitle = params$subtitle,
    show.95 = params$show_95,
    decimals = params$decimals,
    point.size = params$point_size,
    x.angle = params$x_angle,
    y.expand = params$y_expand,
    y.neg = params$y_neg,
    y.percent = params$y_percent,
    show.grid = params$show_grid,
    return.data = TRUE
  )
  cleanup_rplots_pdf()

  list(
    analysis_id = "qic",
    title = "SPC chart",
    subtitle = sprintf("%s sobre %s", params$chart_type, params$y_col),
    summary = list(
      Funcion = "qicharts2::qic",
      Chart = params$chart_type,
      X = non_empty_value(if (identical(params$x_col, "_index")) "Secuencia" else params$x_col),
      Y = params$y_col,
      N = non_empty_value(params$n_col),
      Facet = non_empty_value(params$facet_col),
      Observaciones = nrow(df)
    ),
    tables = list(
      `Resumen del chart` = summary_df,
      `Datos del chart` = detail_df
    ),
    plot_obj = plot_obj,
    build_plot = function(path) {
      build_plot_export(plot_obj, path)
    }
  )
}

#' Ejecuta un analisis de Pareto
#'
#' @param df `data.frame` con los datos de entrada.
#' @param params Lista de parametros construida desde la interfaz.
#'
#' @return Una lista con metadatos, tablas, grafico y helper de exportacion.
run_pareto_analysis <- function(df, params) {
  require_qicharts2()

  x_value <- df[[params$category_col]]
  plot_obj <- suppress_default_plot_file(qicharts2::paretochart(
    x = x_value,
    title = params$title,
    subtitle = params$subtitle,
    ylab = params$ylab,
    xlab = params$xlab,
    x.angle = params$x_angle,
    useNA = params$use_na
  ))
  cleanup_rplots_pdf()

  pareto_table <- build_pareto_table(x_value, use_na = params$use_na)

  list(
    analysis_id = "pareto",
    title = "Pareto chart",
    subtitle = params$category_col,
    summary = list(
      Funcion = "qicharts2::paretochart",
      Variable = params$category_col,
      Categorias = nrow(pareto_table),
      Observaciones = sum(pareto_table$Frecuencia)
    ),
    tables = list(
      `Tabla de Pareto` = pareto_table
    ),
    plot_obj = plot_obj,
    build_plot = function(path) {
      build_plot_export(plot_obj, path)
    }
  )
}

#' Ejecuta un analisis Bernoulli CUSUM
#'
#' @param df `data.frame` con los datos de entrada.
#' @param params Lista de parametros construida desde la interfaz.
#'
#' @return Una lista con metadatos, tablas, grafico y helper de exportacion.
run_bchart_analysis <- function(df, params) {
  require_qicharts2()

  x_value <- coerce_binary_vector(df[[params$outcome_col]])
  plot_obj <- suppress_default_plot_file(qicharts2::bchart(
    x = x_value,
    target = params$target,
    or = params$odds_ratio,
    limit = params$limit,
    title = params$title,
    ylab = params$ylab,
    xlab = params$xlab
  ))
  cleanup_rplots_pdf()

  detail_df <- as.data.frame(plot_obj$data)
  signal_count <- sum(!is.na(detail_df$signal1) | !is.na(detail_df$signal2))

  list(
    analysis_id = "bchart",
    title = "Bernoulli CUSUM",
    subtitle = params$outcome_col,
    summary = list(
      Funcion = "qicharts2::bchart",
      Variable = params$outcome_col,
      Observaciones = length(x_value),
      Target = params$target,
      `Odds ratio` = params$odds_ratio,
      Limite = params$limit,
      `Eventos positivos` = sum(x_value, na.rm = TRUE),
      Senales = signal_count
    ),
    tables = list(
      `Datos del CUSUM` = detail_df
    ),
    plot_obj = plot_obj,
    build_plot = function(path) {
      build_plot_export(plot_obj, path)
    }
  )
}

#' Despacha el wrapper SPC apropiado
#'
#' @param analysis_id Identificador interno del analisis.
#' @param df `data.frame` con los datos de entrada.
#' @param params Lista de parametros del analisis.
#'
#' @return La estructura devuelta por el wrapper seleccionado.
run_spc_analysis <- function(analysis_id, df, params) {
  switch(
    analysis_id,
    qic = run_qic_analysis(df, params),
    pareto = run_pareto_analysis(df, params),
    bchart = run_bchart_analysis(df, params),
    stop(sprintf("Analisis no soportado: %s", analysis_id), call. = FALSE)
  )
}

#' Exporta resultados del analisis a un libro de Excel
#'
#' @param path Ruta del archivo `.xlsx` de salida.
#' @param analysis_result Resultado estructurado del analisis.
#' @param plot_path Ruta opcional a una imagen del grafico.
#' @param interpretation_text Texto opcional con interpretacion adicional.
#'
#' @return La ruta exportada, de forma invisible.
write_analysis_export_workbook <- function(path, analysis_result, plot_path = NULL, interpretation_text = NULL) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop(
      paste(
        "El paquete 'openxlsx' no esta instalado.",
        "Instalalo con install.packages('openxlsx')."
      ),
      call. = FALSE
    )
  }

  wb <- openxlsx::createWorkbook()
  title_style <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#DCE6F1")
  body_style <- openxlsx::createStyle(valign = "top", wrapText = TRUE)

  openxlsx::addWorksheet(wb, "Resumen")
  summary_df <- data_frame_from_named_list(analysis_result$summary)
  openxlsx::writeData(wb, "Resumen", x = summary_df, startRow = 1, startCol = 1, rowNames = FALSE)
  openxlsx::addStyle(wb, "Resumen", title_style, rows = 1, cols = 1:2, gridExpand = TRUE)
  openxlsx::setColWidths(wb, "Resumen", cols = 1:2, widths = "auto")

  for (sheet_name in names(analysis_result$tables)) {
    table_df <- analysis_result$tables[[sheet_name]]
    if (is.null(table_df)) {
      next
    }

    target_sheet <- sanitize_sheet_name(sheet_name)
    openxlsx::addWorksheet(wb, target_sheet)
    openxlsx::writeData(wb, target_sheet, x = table_df, startRow = 1, startCol = 1, rowNames = FALSE)
    openxlsx::addStyle(
      wb,
      target_sheet,
      title_style,
      rows = 1,
      cols = seq_len(max(1, ncol(table_df))),
      gridExpand = TRUE
    )
    openxlsx::setColWidths(wb, target_sheet, cols = seq_len(max(1, ncol(table_df))), widths = "auto")
  }

  openxlsx::addWorksheet(wb, "Grafico")
  if (!is.null(plot_path) && file.exists(plot_path)) {
    openxlsx::insertImage(
      wb,
      sheet = "Grafico",
      file = plot_path,
      startRow = 2,
      startCol = 2,
      width = 10,
      height = 7,
      units = "in"
    )
  } else {
    openxlsx::writeData(
      wb,
      "Grafico",
      x = "No se pudo generar el grafico exportable.",
      startRow = 2,
      startCol = 2
    )
  }

  openxlsx::addWorksheet(wb, "Interpretacion")
  interpretation_value <- if (is.null(interpretation_text) || !nzchar(trimws(interpretation_text))) {
    "No se genero interpretacion para este analisis."
  } else {
    interpretation_text
  }
  openxlsx::writeData(
    wb,
    "Interpretacion",
    x = data.frame(Interpretacion = interpretation_value, check.names = FALSE),
    startRow = 1,
    startCol = 1,
    rowNames = FALSE
  )
  openxlsx::addStyle(wb, "Interpretacion", body_style, rows = 2, cols = 1, gridExpand = TRUE, stack = TRUE)
  openxlsx::setColWidths(wb, "Interpretacion", cols = 1, widths = 120)
  openxlsx::setRowHeights(wb, "Interpretacion", rows = 2, heights = 120)

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}
