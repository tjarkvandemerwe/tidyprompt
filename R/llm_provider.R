#' @title [llm_provider()] R6 Class
#'
#' @description This class provides a structure for creating [llm_provider()]
#' objects with different implementations of the `complete_chat` function. Using
#' this class, you can create an [llm_provider()] object that interacts with
#' different LLM providers, such Ollama, OpenAI, or other custom providers.
#'
#' @export
#'
#' @family llm_provider
llm_provider <- R6::R6Class(
  "llm_provider",
  public = list(
    #' @field parameters A named list of parameters to configure the
    #' [llm_provider()]. Parameters may be appended to the request body when
    #' interacting with the LLM provider API
    parameters = list(),
    #' @field verbose A logical indicating whether interaction with the LLM
    #' provider should be printed to the console
    verbose = getOption("tidyprompt.verbose", TRUE),
    #' @field url The URL to the LLM provider API endpoint for chat completion
    url = NULL,
    #' @field api_key The API key to use for authentication with the LLM
    #' provider API
    api_key = NULL,

    #' @description
    #' Create a new [llm_provider()] object
    #'
    #' @param complete_chat_function Function that will be called by the
    #' [llm_provider()] to complete a chat. This function should take a
    #' `chat_history` data frame as input and return a response object (a list
    #' with `role` and `content`, detailing the chat completion)
    #'
    #' @param parameters A named list of parameters to configure the [llm_provider()].
    #' These parameters may be appended to the request body when interacting with
    #' the LLM provider. For example, the `model` parameter may often be required.
    #' The 'stream' parameter may be used to indicate that the API should stream,
    #' which will be handled down the line by the make_llm_provider_request function.
    #' Parameters should not include the chat_history, as this is passed as a
    #' separate argument to the `complete_chat_function`. Paramters should also
    #' not include 'api_key' or 'url'; these are treated separately
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
    #' @return A new [llm_provider()] R6 object
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

    #' @description Helper function to set the parameters of the [llm_provider()]
    #' object. This function appends new parameters to the existing parameters
    #' list.
    #'
    #' @param new_parameters A named list of new parameters to append to the
    #' existing parameters list
    #'
    #' @return The modified [llm_provider()] object
    set_parameters = function(new_parameters) {
      if (length(new_parameters) > 0 && is.null(names(new_parameters))) {
        stop("new_parameters must be a named list")
      }
      self$parameters <- utils::modifyList(self$parameters, new_parameters)
      return(self)
    },

    #' @description complete_chat function; sends a chat_history to the LLM
    #' provider using the configured `complete_chat_function`. This function is
    #' typically called by the `send_prompt` function to interact with the LLM
    #' provider, but it can also be called directly.
    #'
    #' @param chat_history A data frame with 'role' and 'content' columns
    #'
    #' @return The response from the LLM provider, in a named list
    #' with 'role', 'content', and 'http'. The 'role' and 'content'
    #' fields (required) contain the extracted role and content from the
    #' response (e.g., 'assistant' and 'Hello, how can I help you?').
    #' The 'http' field (optional) may contain any additional information, e.g.,
    #' data from the HTTP response about the number of tokens used.
    complete_chat = function(chat_history) {
      chat_history <- chat_history(chat_history)
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

      # Check that response is valid
      if (
        !is.list(response)
        || !all(c("role", "content") %in% names(response))
      ) {
        stop(paste0(
          "The response from the LLM provider must be a list with 'role' and",
          " 'content' fields."
        ))
      }
      # Check that 'role' and 'content' are character
      if (!all(sapply(response[c("role", "content")], is.character))) {
        stop(paste0(
          "The 'role' and 'content' fields in the response must be of type",
          " character."
        ))
      }
      # Check that there are no other fields in the response
      #   (only allowed: 'role', 'content', and 'response')
      if (length(setdiff(names(response), c("role", "content", "http"))) > 0) {
        stop(paste0(
          "The response from the LLM provider must contain only 'role',",
          " 'content', and 'http' fields."
        ))
      }

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



#' Make a request to an LLM provider
#'
#' Helper function to handle making requests to LLM providers, to be used
#' within a complete_chat() function for a LLM provider.
#'
#' @param url The URL of the LLM provider API endpoint
#' @param headers A list of headers to be passed to the API (can be NULL)
#' @param body The body of the POST request
#' @param stream A logical indicating whether the API should stream responses
#' @param verbose A logical indicating whether the interaction with the LLM provider
#' should be printed to the console. Default is TRUE.
#' @param stream_api_type The type of API to use; specifically required to handle streaming.
#' Currently, "openai" and "ollama" have been implemented. "openai" should also work
#' with other similar APIs for chat completion. If your API handles streaming in a
#' different way, you may need to implement your own version of this function
#' (you are then encouraged to submit a pull request to the GitHub repo of 'tidyprompt').
#'
#' @return A list with the role and content of the response from the LLM provider
#'
#' @export
#' @example inst/examples/llm_provider.R
#'
#' @family llm_provider
make_llm_provider_request <- function(
    url, headers = NULL, body, stream = NULL, verbose = getOption("tidyprompt.verbose", TRUE),
    stream_api_type = c("openai", "ollama")
) {
  if (!is.null(stream) && stream) {
    stream_api_type <- match.arg(stream_api_type)

    role <- NULL
    message <- ""

    write_stream_function <- function(x) {
      if (stream_api_type == "ollama") {
        content <- x |> rawToChar() |> jsonlite::fromJSON()

        if (verbose)
          cat(content$message$content)

        if (is.null(role))
          role <<- content$message$role

        message <<- paste0(message, content$message$content)
      }

      if (stream_api_type == "openai") {
        char <- x |>
          rawToChar() |>
          strsplit(split = "\ndata: ") |>
          unlist()

        parsed_data <- lapply(char, function(chunk) {
          json_text <- sub("^data:\\s*", "", chunk)
          # Use tryCatch to handle potential errors
          tryCatch(
            jsonlite::fromJSON(json_text),
            error = function(e) NULL  # Return NULL if there's an error
          )
        })

        if (is.null(role)) {
          role <<- parsed_data[[1]]$choices$delta$role
        }

        for (data in parsed_data) {
          addition <- data$choices$delta$content

          if (!is.null(addition)) {
            message <<- paste0(message, addition)
            if (verbose)
              cat(addition)
          }
        }
      }
    }

    httr::handle_reset(url)
    response <- httr::POST(
      url,
      httr::add_headers(.headers = headers),
      httr::write_stream(write_stream_function),
      body = body,
      encode = "json"
    )

    if (verbose)
      cat("\n")
  } else {
    response <- httr::POST(
      url,
      httr::add_headers(.headers = headers),
      body = body,
      encode = "json"
    )
  }

  if (httr::status_code(response) != 200)
    stop("Error: ", httr::status_code(response), " - ", httr::content(response, as = "text"))

  # If not streaming, parse the response content
  if (is.null(stream) || isFALSE(stream)) {
    content <- httr::content(response, as = "parsed")

    if (stream_api_type == "ollama") {
      role <- content$message$role
      message <- content$message$content
    } else { # OpenAI type API:
      role <- content$choices[[1]]$message$role
      message <- content$choices[[1]]$message$content
    }
  }

  return(list(
    role = role,
    content = message,
    http = response
  ))
}



#' Create a new Ollama [llm_provider()] instance
#'
#' @param parameters A named list of parameters. Currently the following parameters are required:
#'    - model: The name of the model to use
#'    - stream: A logical indicating whether the API should stream responses
#'  Additional parameters may be passed by adding them to the parameters list;
#'  these parameters will be passed to the Ollama API via the body of the POST request.
#'  Options specifically can be set with the $set_options function (e.g.,
#'  ollama$set_options(list(temperature = 0.8))). See available options at
#'  https://ollama.com/docs/api/chat
#' @param verbose A logical indicating whether the interaction with the LLM provider
#' should be printed to the console
#' @param url The URL to the Ollama API endpoint for chat completion
#' (typically: "http://localhost:11434/api/chat")
#'
#' @return A new [llm_provider()] object for use of the Ollama API
#'
#' @export
#' @example inst/examples/llm_providers.R
#'
#' @family llm_provider
llm_provider_ollama <- function(
    parameters = list(
      model = "llama3.1:8b",
      stream = getOption("tidyprompt.stream", TRUE)
    ),
    verbose = getOption("tidyprompt.verbose", TRUE),
    url = "http://localhost:11434/api/chat"
) {
  complete_chat <- function(chat_history) {
    body <- list(
      model = self$parameters$model,
      messages = lapply(seq_len(nrow(chat_history)), function(i) {
        list(role = chat_history$role[i], content = chat_history$content[i])
      })
    )

    # Append all other parameters to the body
    for (name in names(self$parameters))
      body[[name]] <- self$parameters[[name]]

    return(make_llm_provider_request(
      url = self$url,
      headers = NULL,
      body = body,
      stream = self$parameters$stream,
      verbose = self$verbose,
      stream_api_type = "ollama"
    ))
  }

  if (is.null(parameters$stream))
    parameters$stream <- FALSE

  ollama <- llm_provider$new(
    complete_chat_function = complete_chat,
    parameters = parameters,
    verbose = verbose,
    url = url
  )

  return(ollama)
}



#' Create a new OpenAI [llm_provider()] instance
#'
#' This function creates a new [llm_provider()] that interacts with the Open AI API
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
#' @param url The URL to the OpenAI API endpoint for chat completion
#' (typically: "https://api.openai.com/v1/chat/completions")
#' @param api_key The API key to use for authentication with the OpenAI API
#'
#' @return A new [llm_provider()] object for use of the OpenAI API
#'
#' @export
#' @example inst/examples/llm_providers.R
#'
#' @family llm_provider
llm_provider_openai <- function(
    parameters = list(
      model = "gpt-4o-mini",
      stream = getOption("tidyprompt.stream", TRUE)
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

  return(llm_provider$new(
    complete_chat_function = complete_chat,
    parameters = parameters,
    verbose = verbose,
    url = url,
    api_key = api_key
  ))
}



#' Create a new OpenRouter [llm_provider()] instance
#'
#' @param parameters A named list of parameters. Currently the following parameters are required:
#'    - model: The name of the model to use
#'    - stream: A logical indicating whether the API should stream responses
#'  Additional parameters are appended to the request body; see the OpenRouter API
#'  documentation for more information: https://openrouter.ai/docs/parameters
#' @param verbose A logical indicating whether the interaction with the LLM provider
#' should be printed to the console.
#' @param url The URL to the OpenRouter API endpoint for chat completion
#' @param api_key The API key to use for authentication with the OpenRouter API
#'
#' @return A new [llm_provider()] object for use of the OpenRouter API
#'
#' @export
#' @example inst/examples/llm_providers.R
#'
#' @family llm_provider
llm_provider_openrouter <- function(
    parameters = list(
      model = "qwen/qwen-2.5-7b-instruct",
      stream = getOption("tidyprompt.stream", TRUE)
    ),
    verbose = getOption("tidyprompt.verbose", TRUE),
    url = "https://openrouter.ai/api/v1/chat/completions",
    api_key = Sys.getenv("OPENROUTER_API_KEY")
) {
  llm_provider_openai(parameters, verbose, url, api_key)
}



#' Create a new Mistral [llm_provider()] instance
#'
#' This function creates a new [llm_provider()] that interacts with the Mistral API.
#'
#' @param parameters A named list of parameters. Currently the following parameters are required:
#'    - model: The name of the model to use
#'    - stream: A logical indicating whether the API should stream responses
#'  Additional parameters are appended to the request body; see the Mistral API
#'  documentation for more information: https://docs.mistral.ai/api/#tag/chat
#' @param verbose A logical indicating whether the interaction with the LLM provider
#' should be printed to the consol
#' @param url The URL to the Mistral API endpoint for chat completion
#' @param api_key The API key to use for authentication with the Mistral API
#'
#' @return A new [llm_provider()] object for use of the Mistral API
#'
#' @export
#' @example inst/examples/llm_providers.R
#'
#' @family llm_provider
llm_provider_mistral <- function(
    parameters = list(
      model = "ministral-3b-latest",
      stream = getOption("tidyprompt.stream", TRUE)
    ),
    verbose = getOption("tidyprompt.verbose", TRUE),
    url = "https://api.mistral.ai/v1/chat/completions",
    api_key = Sys.getenv("MISTRAL_API_KEY")
) {
  llm_provider_openai(parameters, verbose, url, api_key)
}



#' Create a new Groq [llm_provider()] instance
#'
#' @param parameters A named list of parameters. Currently the following parameters are required:
#'   - model: The name of the model to use
#'   - stream: A logical indicating whether the API should stream responses
#'  Additional parameters are appended to the request body; see the Groq API
#'  documentation for more information: https://console.groq.com/docs/api-reference#chat-create
#' @param verbose A logical indicating whether the interaction with the LLM provider
#' should be printed to the console
#' @param api_key The API key to use for authentication with the Groq API
#' @param url The URL to the Groq API endpoint for chat completion
#'
#' @return A new [llm_provider()] object for use of the Groq API
#'
#' @export
#' @example inst/examples/llm_providers.R
#'
#' @family llm_provider
llm_provider_groq <- function(
    parameters = list(
      model = "llama-3.1-8b-instant",
      stream = FALSE
    ),
    verbose = getOption("tidyprompt.verbose", TRUE),
    url = "https://api.groq.com/openai/v1/chat/completions",
    api_key = Sys.getenv("GROQ_API_KEY")
) {
  llm_provider_openai(parameters, verbose, url, api_key)
}



#' Create a new XAI (Grok) [llm_provider()] instance
#'
#' @param parameters A named list of parameters. Currently the following parameters are required:
#'   - model: The name of the model to use
#'   - stream: A logical indicating whether the API should stream responses
#'  Additional parameters are appended to the request body; see the XAI API
#'  documentation for more information: https://docs.x.ai/api/endpoints#chat-completions
#' @param verbose A logical indicating whether the interaction with the LLM provider
#' should be printed to the console. Default is TRUE.
#' @param url The URL to the XAI API endpoint for chat completion
#' @param api_key The API key to use for authentication with the XAI API
#'
#' @return A new [llm_provider()] object for use of the XAI API
#'
#' @export
#' @example inst/examples/llm_providers.R
#'
#' @family llm_provider
llm_provider_xai <- function(
    parameters = list(
      model = "grok-beta",
      stream = getOption("tidyprompt.stream", TRUE)
    ),
    verbose = getOption("tidyprompt.verbose", TRUE),
    url = "https://api.x.ai/v1/chat/completions",
    api_key = Sys.getenv("XAI_API_KEY")
) {
  llm_provider_openai(parameters, verbose, url, api_key)
}



#' Create a new Google Gemini [llm_provider()] instance
#'
#' Creates an [llm_provider()] object that interacts with the Google Gemini API.
#' Streaming is not yet supported in this implementation.
#'
#' @param parameters A named list of parameters. Currently the following parameters are required:
#'    - model: The name of the model to use (see: https://ai.google.dev/gemini-api/docs/models/gemini)
#'  Additional parameters are appended to the request body; see the Google AI Studio API
#'  documentation for more information: https://ai.google.dev/gemini-api/docs/text-generation
#'  & https://github.com/google/generative-ai-docs/blob/main/site/en/gemini-api/docs/get-started/rest.ipynb
#' @param verbose A logical indicating whether the interaction with the LLM provider
#' should be printed to the console
#' @param url The URL to the Google Gemini API endpoint for chat completion
#' @param api_key The API key to use for authentication with the Google Gemini API
#' (see: https://aistudio.google.com/app/apikey)
#'
#' @return A new [llm_provider()] object for use of the Google Gemini API
#'
#' @export
#' @example inst/examples/llm_providers.R
#'
#' @family llm_provider
llm_provider_google_gemini <- function(
    parameters = list(
      model = "gemini-1.5-flash"
    ),
    verbose = getOption("tidyprompt.verbose", TRUE),
    url = "https://generativelanguage.googleapis.com/v1beta/models/",
    api_key = Sys.getenv("GOOGLE_AI_STUDIO_API_KEY")
) {
  complete_chat <- function(chat_history) {
    # Construct URL for the API request
    url <- paste0(
      self$url,
      self$parameters$model,
      ":generateContent",
      "?key=", self$api_key
    )

    headers <- c(
      "Content-Type" = "application/json"
    )

    # Format chat_history for API compatibility with the 'contents' format
    formatted_contents <- lapply(seq_len(nrow(chat_history)), function(i) {
      list(
        role = ifelse(chat_history$role[i] == "assistant", "model", chat_history$role[i]),
        parts = list(list(text = chat_history$content[i]))
      )
    })

    # Build the request body with 'contents' field
    body <- list(
      contents = formatted_contents
    )

    # Append all other parameters to the body
    for (name in names(self$parameters))
      body[[name]] <- self$parameters[[name]]


    # Send the POST request with the properly formatted body
    response <- httr::POST(url, httr::add_headers(.headers = headers), body = body, encode = "json")

    # Check if the request was successful
    if (httr::status_code(response) == 200) {
      content <- httr::content(response, as = "parsed")

      return(list(
        role = "assistant",
        content = content$candidates[[1]]$content$parts[[1]]$text
      ))
    } else {
      stop("Error: ", httr::status_code(response), " - ", httr::content(response, as = "text"))
    }
  }

  llm_provider$new(
    complete_chat_function = complete_chat,
    parameters = parameters,
    verbose = verbose,
    url = url,
    api_key = api_key
  )
}



#' Create a fake [llm_provider()] (for development and testing purposes)
#'
#' This function creates a fake [llm_provider()] that can be used for development
#' and testing purposes. It is hardcoded to send back specific responses to
#' specific prompts that are used in vignettes, tests, and examples.
#' This is useful for running tests and builds in environments in which an
#' actual [llm_provider()] is not available.
#'
#' @param verbose A logical indicating whether the interaction with the [llm_provider()]
#' should be printed to the console. Default is TRUE.
#'
#' @return A new [llm_provider()] object for use of the fake LLM provider
#'
#' @noRd
#' @keywords internal
llm_provider_fake <- function(verbose = getOption("tidyprompt.verbose", TRUE)) {
  complete_chat <- function(chat_history) {
    last_msg <- tail(chat_history$content, 1)

    answer_as_integer_input <-
      "You must answer with only an integer (use no other characters)."

    chain_of_thought_input <-
      "To answer the user's prompt, you need to think step by step to arrive at a final answer."

    if (last_msg == "Hi there!") {
      return(list(
        role = "assistant",
        content = paste0(
          "It's nice to meet you.",
          " Is there something I can help you with or would you like to chat?"
        )
      ))
    }

    if (grepl(
      "What is a large language model? Explain in 10 words.", last_msg,
      fixed = TRUE
    )) {
      return(list(
        role = "assistant",
        content = paste0(
          "Complex computer program trained on vast texts to generate human-like",
          " responses."
        )
      ))
    }

    if (
      grepl("What is 2 + 2?", last_msg, fixed = TRUE)
      & grepl(answer_as_integer_input, last_msg, fixed = TRUE)
      & !grepl(chain_of_thought_input, last_msg, fixed = TRUE)
    ) {
      return(list(
        role = "assistant",
        content = "4"
      ))
    }

    if (
      grepl("What is 2 + 2?", last_msg, fixed = TRUE)
      & !grepl(answer_as_integer_input, last_msg, fixed = TRUE)
    ) {
      return(list(
        role = "assistant",
        content = "Four."
      ))
    }

    if (
      any(grepl("What is 2 + 2?", chat_history$content[chat_history$role == "user"], fixed = TRUE))
      & grepl(answer_as_integer_input, last_msg, fixed = TRUE)
      & !grepl(chain_of_thought_input, last_msg, fixed = TRUE)
    ) {
      return(list(
        role = "assistant",
        content = "4"
      ))
    }

    if (
      grepl("What is 2 + 2?", last_msg,  fixed = TRUE)
      & grepl(chain_of_thought_input, last_msg,  fixed = TRUE)
      & grepl(answer_as_integer_input, last_msg,  fixed = TRUE)
    ) {
      return(list(
        role = "assistant",
        content = glue::glue(
          ">> step 1: Identify the mathematical operation in the prompt,
          which is a simple addition problem.

          >> step 2: Recall the basic arithmetic fact that 2 + 2 equals a specific
          numerical value.

          >> step 3: Apply this knowledge to determine the result of the addition problem,
          using the known facts about numbers and their operations.

          >> step 4: Conclude that based on this mathematical understanding, the
          solution to the prompt \"What is 2 + 2?\" is a fixed numerical quantity.

          FINISH[4]"
        )
      ))
    }

    if (grepl(
      'example usage: FUNCTION[temperature_in_location]("Amsterdam", "Fahrenheit")',
      last_msg,
      fixed = TRUE
    )) {
      return(list(
        role = "assistant",
        content = glue::glue(
          "I'll use the provided function to get the current temperature in Enschede.

          FUNCTION[temperature_in_location](\"Enschede\", \"Celcius\")"
        )
      ))
    }

    if (
      grepl("function called: temperature_in_location", last_msg, fixed = TRUE)
      & grepl("arguments used: location = Enschede", last_msg, fixed = TRUE)
    ) {
      return(list(
        role = "assistant",
        content = "So the current temperature in Enschede is 22.7 degrees Celsius."
      ))
    }

    if (
      any(grepl(
        "So the current temperature in Enschede is 22.7 degrees Celsius.",
        chat_history$content[chat_history$role == "assistant"],
        fixed = TRUE
      ))
      & grepl(last_msg, answer_as_integer_input, fixed = TRUE)
    ) {
      return(list(
        role = "assistant",
        content = "22"
      ))
    }

    return(list(
      role = "assistant",
      content = "I'm a fake LLM! This is my default response."
    ))
  }

  llm_provider$new(
    complete_chat_function = complete_chat,
    verbose = verbose,
    parameters = list(
      model = 'llama3.1:8b'
    )
  )
}
