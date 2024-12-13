#' @title LlmProvider R6 Class
#'
#' @description This class provides a structure for creating [llm_provider-class]
#'  objects with different implementations of `$complete_chat()`.
#' Using this class, you can create an [llm_provider-class] object that interacts
#'  with different LLM providers, such Ollama, OpenAI, or other custom providers.
#'
#' @export
#'
#' @family llm_provider
`llm_provider-class` <- R6::R6Class(
  "LlmProvider",
  public = list(
    #' @field parameters
    #' A named list of parameters to configure the [llm_provider-class].
    #' Parameters may be appended to the request body when interacting with the
    #'  LLM provider API
    parameters = list(),

    #' @field verbose
    #' A logical indicating whether interaction with the LLM provider should be
    #'  printed to the console
    verbose = getOption("tidyprompt.verbose", TRUE),

    #' @field url
    #' The URL to the LLM provider API endpoint for chat completion
    url = NULL,

    #' @field api_key
    #' The API key to use for authentication with the LLM provider API
    api_key = NULL,

    #' @field api_type
    #' The type of API to use (e.g., "openai", "ollama").
    #'  This is used to determine certain specific behaviors for different APIs,
    #'  for instance, as is done in the [answer_as_json()] function
    api_type = "unspecified",

    #' @field handler_fns
    #' A list of functions that will be called after the completion of a chat.
    #'  See `$add_handler_fn()`
    handler_fns = list(),

    #' @description
    #' Create a new [llm_provider-class] object
    #'
    #' @param complete_chat_function Function that will be called by the
    #' [llm_provider-class] to complete a chat.
    #' This function should take a list containing at least '$chat_history'
    #'  (a data frame with 'role' and 'content' columns) and return a response
    #'  object, which contains:
    #'  \itemize{
    #'  \item 'completed': A dataframe with 'role' and 'content' columns,
    #'  containing the completed chat history
    #'
    #'  \item 'http': A list containing a list 'requests' and a list 'responses',
    #'  containing the HTTP requests and responses made during the chat completion
    #'  }
    #'
    #' @param parameters
    #' A named list of parameters to configure the [llm_provider-class].
    #'  These parameters may be appended to the request body when interacting with
    #'  the LLM provider.
    #' For example, the `model` parameter may often be required.
    #'  The 'stream' parameter may be used to indicate that the API should stream.
    #' Parameters should not include the chat_history, or 'api_key' or 'url', which
    #'  are handled separately by the [llm_provider-class] and '$complete_chat()'.
    #' Parameters should also not be set when they are handled by prompt wraps
    #'
    #' @param verbose A logical indicating whether interaction with the LLM
    #' provider should be printed to the console
    #'
    #' @param url The URL to the LLM provider API endpoint for chat completion
    #' (typically required, but may be left NULL in some cases, for instance
    #' when creating a fake LLM provider)
    #'
    #' @param api_key The API key to use for authentication with the LLM
    #' provider API (optional, not required for, for instance, Ollama)
    #'
    #' @param api_type The type of API to use (e.g., "openai", "ollama").
    #' This is used to determine certain specific behaviors for different APIs
    #' (see for example the [answer_as_json()] function)
    #'
    #' @return A new [llm_provider-class] R6 object
    initialize = function(
      complete_chat_function,
      parameters = list(),
      verbose = TRUE,
      url = NULL,
      api_key = NULL,
      api_type = "unspecified"
    ) {
      if (length(parameters) > 0 && is.null(names(parameters)))
        stop("parameters must be a named list")

      private$complete_chat_function <- complete_chat_function
      self$parameters <- parameters
      self$verbose <- verbose
      self$url <- url
      self$api_key <- api_key
      self$api_type <- api_type
    },

    #' @description Helper function to set the parameters of the [llm_provider-class]
    #' object. This function appends new parameters to the existing parameters
    #' list.
    #'
    #' @param new_parameters A named list of new parameters to append to the
    #' existing parameters list
    #'
    #' @return The modified [llm_provider-class] object
    set_parameters = function(new_parameters) {
      if (length(new_parameters) == 0)
        return(self)

      stopifnot(
        is.list(new_parameters),
        length(new_parameters) > 0,
        !is.null(names(new_parameters))
      )
      self$parameters <- utils::modifyList(self$parameters, new_parameters)
      return(self)
    },

    #' @description complete_chat function; sends a chat_history to the LLM
    #' provider using the configured `complete_chat_function`. This function is
    #' typically called by the `send_prompt` function to interact with the LLM
    #' provider, but it can also be called directly.
    #'
    #' @param input A string, a data frame which is a valid chat history
    #' (see [chat_history()]), or a list containing a valid chat history under key
    #' '#chat_history'
    #'
    #' @return The response from the LLM provider, in a named list
    #' with 'role', 'content', and 'http'. The 'role' and 'content'
    #' fields (required) contain the extracted role and content from the
    #' response (e.g., 'assistant' and 'Hello, how can I help you?').
    #' The 'http' field (optional) may contain any additional information, e.g.,
    #' data from the HTTP response about the number of tokens used.
    complete_chat = function(input) {
      if (length(input) == 1 & is.character(input)) {
        chat_history <- chat_history(input)
        input <- list(chat_history = chat_history)
      } else if (is.data.frame(input)) {
        chat_history <- chat_history(input)
        input <- list(chat_history = chat_history)
      }

      stopifnot(
        is.list(input), "chat_history" %in% names(input)
      )

      chat_history <- chat_history(input$chat_history)
      if (self$verbose) {
        message(crayon::bold(glue::glue(
          "--- Sending request to LLM provider",
          " ({
              if (!is.null(self$parameters$model)) {
                self$parameters$model
              } else {
                'no model specified'
              }
          }):",
          " ---"
        )))
        cat(chat_history$content[nrow(chat_history)])
        cat("\n")
      }

      if (self$verbose)
        message(crayon::bold(glue::glue(
          "--- Receiving response from LLM provider: ---"
        )))

      environment(private$complete_chat_function) <- environment()
      response <- private$complete_chat_function(chat_history)

      # Filter content with empty string ("") (Ollama tool call)
      response$completed <- response$completed[response$completed$content != "", ]

      http <- list()
      http$requests[[1]] <- response$http$request
      http$responses[[1]] <- response$http$response

      while (TRUE) {
        for (handler_fn in self$handler_fns) {
          response <- handler_fn(response, self)
          http$requests[[length(http$requests) + 1]] <- response$http$request
          http$responses[[length(http$responses) + 1]] <- response$http$response

          stopifnot(
            is.list(response), "completed" %in% names(response),
            is.data.frame(response$completed),
            all(c("role", "content") %in% names(response$completed))
          )

          if (isTRUE(response$`break`))
            break
        }

        if (!isFALSE(response$done) | isTRUE(response$`break`)) {
          break
        }
      }

      # Update http list
      response$http <- http

      # Print difference between chat_history and completed
      if (
        self$verbose
        && (is.null(self$parameters$stream) || !self$parameters$stream)
      ) {
        chat_history_new <- response$completed[
          (nrow(chat_history) + 1):nrow(response$completed),
        ]

        for (i in seq_len(nrow(chat_history_new))) {
          cat(chat_history_new$content[i], "\n")
        }
      }

      if (isTRUE(response$`break`))
        warning(paste0(
          "Chat completion was interrupted by a handler break"
        ))

      if (self$verbose)
        return(invisible(response))

      return(response)
    },

    #' @description
    #' Helper function to add a handler function to the
    #'  [llm_provider-class] object.
    #' Handler functions are called after the completion of a chat and can be
    #'  used to modify the response before it is returned by the [llm_provider-class].
    #' Each handler function should take the response object
    #'  as input (first argument) as well as 'self' (the [llm_provider-class]
    #'  object) and return a modified response object.
    #' The functions will be called in the order they are added to the list.
    #'
    #' @details
    #' If a handler function returns a list with a 'break' field set to `TRUE`,
    #'  the chat completion will be interrupted and the response will be returned
    #'  at that point.
    #' If a handler function returns a list with a 'done' field set to `FALSE`,
    #'  the handler functions will continue to be called in a loop until the 'done'
    #'  field is not set to `FALSE`.
    #'
    #' @param handler_fn A function that takes the response object plus
    #'  'self' (the [llm_provider-class] object) as input and
    #'  returns a modified response object
    add_handler_fn = function(handler_fn) {
      stopifnot(is.function(handler_fn))
      self$handler_fns <- c(self$handler_fns, list(handler_fn))
      return(self)
    },

    #' @description Helper function to set the handler functions of the
    #' [llm_provider-class] object. This function replaces the existing
    #' handler functions list with a new list of handler functions. See
    #' '$add_handler_fn()' for more information on handler functions
    #'
    #' @param handler_fns A list of handler functions to set
    set_handler_fns = function(handler_fns) {
      stopifnot(is.list(handler_fns))
      self$handler_fns <- handler_fns
      return(self)
    }
  ),
  private = list(
    complete_chat_function = NULL
  )
)
