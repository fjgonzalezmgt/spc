#' @title Helpers para interpretar resultados con OpenAI
#'
#' @description
#' Reune utilidades para serializar resultados del wrapper SPC, codificar
#' imagenes y construir solicitudes a la API de OpenAI.
#'
#' @keywords internal
NULL

#' Reduce un resultado de analisis a una estructura serializable
#'
#' @param analysis_result Lista devuelta por los wrappers de analisis SPC.
#'
#' @return Una lista con campos estables aptos para serializacion JSON.
sanitize_analysis_result <- function(analysis_result) {
  list(
    analysis_id = analysis_result$analysis_id,
    title = analysis_result$title,
    subtitle = analysis_result$subtitle,
    summary = analysis_result$summary,
    tables = lapply(analysis_result$tables, function(tbl) {
      if (is.null(tbl)) {
        return(NULL)
      }
      as.data.frame(tbl)
    })
  )
}

#' Codifica una imagen como data URL en base64
#'
#' @param image_path Ruta local a una imagen soportada.
#'
#' @return Una cadena `data:` lista para enviarse a la API de OpenAI.
encode_image_data_url <- function(image_path) {
  if (!requireNamespace("base64enc", quietly = TRUE)) {
    stop("Falta instalar 'base64enc'. Usa install.packages('base64enc').", call. = FALSE)
  }

  ext <- tolower(tools::file_ext(image_path))
  mime_type <- switch(
    ext,
    png = "image/png",
    jpg = "image/jpeg",
    jpeg = "image/jpeg",
    webp = "image/webp",
    stop("Formato de imagen no soportado para OpenAI.", call. = FALSE)
  )

  paste0("data:", mime_type, ";base64,", base64enc::base64encode(image_path))
}

#' Extrae texto util de una respuesta de la API de OpenAI
#'
#' @param body Cuerpo decodificado de la respuesta JSON.
#'
#' @return Una cadena con el texto consolidado o `NULL` si no hay texto usable.
extract_response_text <- function(body) {
  if (!is.null(body$output_text) && is.character(body$output_text) && nzchar(body$output_text)) {
    return(body$output_text)
  }

  if (!is.null(body$output) && length(body$output) > 0) {
    text_chunks <- unlist(
      lapply(body$output, function(item) {
        if (!is.list(item) || !identical(item$type, "message") || is.null(item$content)) {
          return(character())
        }

        unlist(
          lapply(item$content, function(content_item) {
            if (
              is.list(content_item) &&
              identical(content_item$type, "output_text") &&
              !is.null(content_item$text)
            ) {
              content_item$text
            } else {
              character()
            }
          }),
          use.names = FALSE
        )
      }),
      use.names = FALSE
    )

    text_chunks <- text_chunks[nzchar(text_chunks)]
    if (length(text_chunks) > 0) {
      return(paste(text_chunks, collapse = "\n\n"))
    }
  }

  NULL
}

#' Solicita una interpretacion textual de un analisis SPC
#'
#' @param analysis_result Lista devuelta por un wrapper de analisis SPC.
#' @param plot_path Ruta opcional al grafico exportado.
#' @param language Idioma objetivo para la interpretacion.
#' @param extra_instructions Instrucciones adicionales que se agregan al
#'   prompt principal.
#'
#' @return Texto interpretativo generado por OpenAI. Si la respuesta no incluye
#'   texto directo, devuelve el JSON formateado.
#' @export
openai_interpret_analysis <- function(analysis_result, plot_path = NULL, language = "es", extra_instructions = "") {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Falta instalar 'httr2'. Usa install.packages('httr2').", call. = FALSE)
  }

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Falta instalar 'jsonlite'. Usa install.packages('jsonlite').", call. = FALSE)
  }

  api_key <- Sys.getenv("OPENAI_API_KEY", unset = "")
  model <- Sys.getenv("OPENAI_MODEL", unset = "gpt-5-mini")

  if (!nzchar(api_key)) {
    stop(
      "OPENAI_API_KEY no esta configurada. Copia .Renviron.example a .Renviron y agrega tu clave.",
      call. = FALSE
    )
  }

  prompt_text <- paste(
    "Interpreta el siguiente resultado de un analisis SPC generado con qicharts2 en", language, ".",
    "Devuelve solo lo necesario con este formato:",
    "1. Resumen ejecutivo",
    "2. Hallazgos clave",
    "3. Riesgo o estabilidad del proceso",
    "4. Recomendacion",
    "Cada seccion debe ser breve, clara y orientada a decision.",
    "Usa el resumen numerico, las tablas y el grafico si esta disponible.",
    "No repitas toda la tabla ni inventes supuestos no visibles.",
    if (nzchar(extra_instructions)) extra_instructions else "",
    "\n\nResultado:\n",
    jsonlite::toJSON(sanitize_analysis_result(analysis_result), auto_unbox = TRUE, pretty = TRUE, null = "null")
  )

  message_content <- list(list(type = "input_text", text = prompt_text))

  if (!is.null(plot_path) && file.exists(plot_path)) {
    message_content[[length(message_content) + 1]] <- list(
      type = "input_image",
      image_url = encode_image_data_url(plot_path),
      detail = "high"
    )
  }

  payload <- list(
    model = model,
    instructions = paste(
      "Actua como especialista en control estadistico de procesos y mejora continua.",
      "Explica la estabilidad o senales especiales del proceso en espanol claro y conciso.",
      "Usa el grafico adjunto como apoyo visual cuando exista."
    ),
    input = list(
      list(
        role = "user",
        content = message_content
      )
    ),
    text = list(
      verbosity = "low"
    )
  )

  response <- httr2::request("https://api.openai.com/v1/responses") |>
    httr2::req_headers(
      Authorization = paste("Bearer", api_key),
      "Content-Type" = "application/json"
    ) |>
    httr2::req_body_json(payload, auto_unbox = TRUE) |>
    httr2::req_perform()

  body <- httr2::resp_body_json(response, simplifyVector = FALSE)
  text <- extract_response_text(body)

  if (!is.null(text) && nzchar(text)) {
    return(text)
  }

  jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE, null = "null")
}
