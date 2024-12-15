#' @title Make a request to an LLM provider
#'
#' @description This is a helper function which facilitates making requests to LLM
#' providers which follow the structure of the OpenAI API or the Ollama
#' API. It handles both streaming and non-streaming requests.
#'
#' This function is part of the internal API and is not intended to be called directly by
#' users. It is used in some of the pre-built [llm_provider-class] objects
#' included in 'tidyprompt' (e.g., [llm_provider_openai()) and
#' [llm_provider_ollama()]).
#'
#' @param chat_history A data frame with 'role' and 'content' columns
#' (see [chat_history()])
#' @param request A 'httr2' request object with the URL, headers, and body
#' @param stream Logical indicating whether the API should stream responses
#' @param verbose Logical indicating whether interactions should be printed to the console
#' @param api_type API type, one of "openai" or "ollama"
#'
#' @return A list with the completed chat history and the HTTP request and response
#' objects
#'
#' @keywords internal
#' @noRd
request_llm_provider <- function(
    chat_history,
    request,
    stream = NULL,
    verbose = getOption("tidyprompt.verbose", TRUE),
    api_type = c("openai", "ollama")
) {
  api_type <- match.arg(api_type)

  if (!is.null(stream) && stream) {
    req_result <- req_llm_stream(request, api_type, verbose)
  } else {
    req_result <- req_llm_non_stream(request, api_type, verbose)
  }

  stopifnot(
    is.list(req_result),
    "new" %in% names(req_result),
    is.data.frame(req_result$new)
  )

  completed <- dplyr::bind_rows(chat_history, req_result$new)

  return(list(
    completed = completed,
    http = list(
      request = request,
      response = req_result$httr2_response
    )
  ))
}

req_llm_handle_error <- function(e) {
  message("Error: ", e$message)
  tryCatch(
    {
      body <- e$resp |>
        httr2::resp_body_string() |>
        jsonlite::fromJSON()
      print(body)
    },
    error = function(e) message("(Could not parse JSON body from response)")
  )
  message("Use 'httr2::last_response()' and 'httr2::last_request()' for more information")
  stop("Could not perform request to LLM provider")
}

req_llm_stream <- function(req, api_type, verbose) {
  role <- NULL
  message_accumulator <- ""
  tool_calls <- list()

  callback_func <- function(chunk) {
    if (api_type == "ollama") {
      parsed_data <- parse_ollama_stream_chunk(chunk)

      for (data in parsed_data) {
        if (is.null(role)) role <<- data$message$role
        message_accumulator <<- paste0(message_accumulator, data$message$content)
        if (verbose) cat(data$message$content)
      }
    }

    if (api_type == "openai") {
      parsed_data <- parse_openai_stream_chunk(chunk)

      for (data in parsed_data) {
        # Set role if not yet set
        if (is.null(role) && !is.null(data$choices$delta$role)) {
          role <<- data$choices$delta$role
        }

        # Handle tool calls
        if (!is.null(data$choices$delta$tool_calls) && length(data$choices$delta$tool_calls) > 0) {
          tool_calls <<- append_or_update_tool_calls(tool_calls, data$choices$delta$tool_calls, verbose)
          # If a tool call is found, no direct message content is appended this chunk
          next
        }

        # Handle regular content
        addition <- data$choices$delta$content
        if (!is.null(addition)) {
          message_accumulator <<- paste0(message_accumulator, addition)
          if (verbose) cat(addition)
        }
      }
    }

    return(TRUE)
  }

  response <- tryCatch(
    httr2::req_perform_stream(
      req, buffer_kb = 0.001, round = "line", callback = callback_func
    ),
    error = function(e) req_llm_handle_error(e)
  )

  if (!is.list(response$body)) response$body <- list()
  response$body$tool_calls <- tool_calls

  if (verbose) cat("\n")

  return(list(
    new = data.frame(role = role, content = message_accumulator),
    httr2_response = response
  ))
}

req_llm_non_stream <- function(req, api_type, verbose) {
  response <- tryCatch(
    httr2::req_perform(req),
    error = function(e) req_llm_handle_error(e)
  )

  content <- httr2::resp_body_json(response)

  if (api_type == "ollama") {
    new <- tryCatch(
      data.frame(
        role = content$message$role,
        content = content$message$content
      ),
      error = function(e) data.frame()
    )
  } else {
    # OpenAI type API
    new <- tryCatch(
      data.frame(
        role = content$choices[[1]]$message$role,
        content = content$choices[[1]]$message$content
      ),
      error = function(e) data.frame()
    )
  }

  return(list(
    new = new,
    httr2_response = response
  ))
}

parse_ollama_stream_chunk <- function(chunk) {
  # Each chunk is separated by newline
  lines <- rawToChar(chunk) |> strsplit("\n") |> unlist()

  parsed <- lapply(lines, function(x) {
    content <- tryCatch(jsonlite::fromJSON(x), error = function(e) NULL)
    # if (!is.null(content$message$content)) {
    #   list(role = content$message$role, content = content$message$content)
    # } else {
    #   NULL
    # }
  })

  Filter(Negate(is.null), parsed)
}

parse_openai_stream_chunk <- function(chunk) {
  # Each chunk may contain multiple lines of streaming data
  char <- rawToChar(chunk) |> strsplit(split = "\ndata: ") |> unlist()

  parsed_data <- lapply(char, function(x) {
    json_text <- sub("^data:\\s*", "", x)
    tryCatch(jsonlite::fromJSON(json_text), error = function(e) NULL)
  })

  Filter(Negate(is.null), parsed_data)
}

append_or_update_tool_calls <- function(tool_calls, new_tool_calls, verbose) {
  tool_call <- new_tool_calls[[1]]
  id <- tool_call$id

  last_id <- if (length(tool_calls) > 0) tool_calls[[length(tool_calls)]]$id else NULL

  if (!is.null(id) & (is.null(last_id) || (id != last_id))) {
    if (verbose & !is.null(last_id)) cat("\n\n")
    tool_calls <- append(tool_calls, list(list(
      id = tool_call$id,
      type = tool_call$type,
      `function` = list(
        name = tool_call$`function`$name,
        args = tool_call$`function`$arguments
      )
    )))

    if (verbose) {
      cat(glue::glue("Calling tool '{tool_call$`function`$name}', with arguments:"))
      cat("\n")
      if (verbose & length(tool_call$`function`$arguments) > 0)
        cat(tool_call$`function`$arguments)
    }
  } else {
    # Update arguments of the last call
    arguments_current <- tool_calls[[length(tool_calls)]]$`function`$arguments
    arguments_new <- tool_call$`function`$arguments
    if (length(arguments_new) > 0) {
      tool_calls[[length(tool_calls)]]$`function`$arguments <- paste0(arguments_current, arguments_new)
      if (verbose) cat(arguments_new)
    }
  }

  tool_calls
}
