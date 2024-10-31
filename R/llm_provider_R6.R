#' @title llmProvider R6 Class
#'
#' @description This class provides a structure for creating `llmProvider` objects with
#' different implementations of the `complete_chat` function. Using this
#' class, you can create an `llmProvider` object that interacts with different
#' LLM providers, such Ollama, OpenAI, or other custom providers.
#'
#' @export
llmProvider <- R6::R6Class(
  "llmProvider",
  public = list(
    #' @field parameters A named list of parameters to configure the `llm_provider`.
    #' Parameters may be appended to the request body when interacting with the
    #' LLM provider API.
    parameters = list(),
    #' @field verbose A logical indicating whether interaction with the LLM provider
    #' should be printed to the console
    verbose = getOption("tidyprompt.verbose", TRUE),
    #' @field url The URL to the LLM provider API endpoint for chat completion
    url = NULL,
    #' @field api_key The API key to use for authentication with the LLM provider API
    api_key = NULL,

    #' @description
    #' Create a new `llmProvider` object
    #'
    #' @param complete_chat_function Function that will be called by the `llm_provider`
    #' to complete a chat. This function should take a `chat_history` data frame
    #' as input and return a response object (a list with `role` and `content`,
    #' detailing the chat completion)
    #'
    #' @param parameters A named list of parameters to configure the `llm_provider`.
    #' These parameters may be appended to the request body when interacting with
    #' the LLM provider. For example, the `model` parameter may often be required.
    #' The 'stream' parameter may be used to indicate that the API should stream,
    #' which will be handled down the line by the make_llm_provider_request function.
    #' Parameters should not include the chat_history, as this is passed as a
    #' separate argument to the `complete_chat_function`. Paramters should also
    #' not include 'api_key' or 'url'; these are treated separately
    #'
    #' @param verbose A logical indicating whether interaction with the LLM provider
    #' should be printed to the console
    #'
    #' @param url The URL to the LLM provider API endpoint for chat completion
    #' (typically required, but may be left NULL in some cases, for instance when
    #' creating a fake LLM provider)
    #'
    #' @param api_key The API key to use for authentication with the LLM provider API
    #' (optional, not required for, for instance, Ollama)
    #'
    #' @return A new `llmProvider` R6 object.
    initialize = function(
      complete_chat_function,
      parameters = list(),
      verbose = TRUE,
      url = NULL,
      api_key = NULL
    ) {
      if (length(parameters) > 0 && is.null(names(parameters)))
        stop("parameters must be a named list")

      private$complete_chat_function <- complete_chat_function
      self$parameters <- parameters
      self$verbose <- verbose
      self$url <- url
      self$api_key <- api_key
    },

    #' @description Helper function to set the parameters of the llmProvider
    #' object. This function appends new parameters to the existing parameters
    #' list.
    #'
    #' @param new_parameters A named list of new parameters to append to the
    #' existing parameters list
    #'
    #' @return The modified `llmProvider` object
    set_parameters = function(new_parameters) {
      if (length(new_parameters) > 0 && is.null(names(new_parameters))) {
        stop("new_parameters must be a named list")
      }
      self$parameters <- utils::modifyList(self$parameters, new_parameters)
      return(self)
    },

    #' @description complete_chat function; sends a chat_history to the LLM provider
    #' using the configured `complete_chat_function`. This function is typically
    #' called by the `send_prompt` function to interact with the LLM provider,
    #' but it can also be called directly.
    #'
    #' @param chat_history A data frame with 'role' and 'content' columns
    #'
    #' @return The response from the LLM provider, in a named list
    #' with 'role' and 'content'
    complete_chat = function(chat_history) {
      chat_history <- chat_history(chat_history)
      if (self$verbose) {
        message(crayon::bold(glue::glue(
          "--- Sending request to LLM provider ({self$parameters$model}): ---"
        )))
        cat(chat_history$content[nrow(chat_history)])
        cat("\n")
      }

      if (self$verbose)
        message(crayon::bold(glue::glue("--- Receiving response from LLM provider: ---")))

      environment(private$complete_chat_function) <- environment()
      response <- private$complete_chat_function(chat_history)

      if (
        self$verbose
        && (is.null(self$parameters$stream) || !self$parameters$stream)
      ) {
        message(response$content)
      }


      if (self$verbose)
        return(invisible(response))

      return(response)
    }
  ),
  private = list(
    complete_chat_function = NULL
  )
)



#' Create a new OpenAI llm_provider instance
#'
#' This function creates a new llm_provider that interacts with the Open AI API
#'
#' @param parameters A named list of parameters. Currently the following parameters are required:
#'    - model: The name of the model to use
#'    - api_key: The API key to use for authentication with the OpenAI API. This
#'    should be a project API key (not a user API key)
#'    - url: The URL to the OpenAI API (may also be an alternative endpoint
#'    that provides a similar API.)
#'    - stream: A logical indicating whether the API should stream responses
#'  Additional parameters are appended to the request body; see the OpenAI API
#'  documentation for more information: https://platform.openai.com/docs/api-reference/chat
#' @param verbose A logical indicating whether the interaction with the LLM provider
#' should be printed to the console. Default is TRUE.
#' @param url The URL to the OpenAI API
#' @param api_key The API key to use for authentication with the OpenAI API
#'
#' @return A new llm_provider object for use of the OpenAI API
#'
#' @export
llm_provider_openai <- function(
    parameters = list(
      model = "gpt-4o-mini",
      stream = TRUE
    ),
    verbose = getOption("tidyprompt.verbose", TRUE),
    url = "https://api.openai.com/v1/chat/completions",
    api_key = Sys.getenv("OPENAI_API_KEY")
) {
  complete_chat <- function(chat_history) {
    headers <- c(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", self$api_key)
    )

    # Prepare the body by converting chat_history dataframe to list of lists
    body <- list(
      messages = lapply(seq_len(nrow(chat_history)), function(i) {
        list(role = chat_history$role[i], content = chat_history$content[i])
      })
    )

    # Append all other parameters to the body
    for (name in names(self$parameters))
      body[[name]] <- self$parameters[[name]]

    make_llm_provider_request(
      url = self$url,
      headers = headers,
      body = body,
      stream = self$parameters$stream,
      verbose = self$verbose,
      stream_api_type = "openai"
    )
  }

  return(llmProvider$new(
    complete_chat_function = complete_chat,
    parameters = parameters,
    verbose = verbose,
    url = url,
    api_key = api_key
  ))
}
