#' Create a new Ollama LLM provider
#'
#' This function creates a new [llm_provider-class] object that interacts with the Ollama API.
#'
#' @param parameters A named list of parameters. Currently the following parameters are required:
#'    - model: The name of the model to use
#'    - stream: A logical indicating whether the API should stream responses
#'
#'  Additional parameters may be passed by adding them to the parameters list;
#'  these parameters will be passed to the Ollama API via the body of the POST request.
#'  Note that various Ollama options need to be set in a list named 'options' within
#'  the parameters list (e.g., context window size is represented in $parameters$options$num_ctx)
#   See available settings at https://github.com/ollama/ollama/blob/main/docs/api.md
#' @param verbose A logical indicating whether the interaction with the LLM provider
#' should be printed to the console
#' @param url The URL to the Ollama API endpoint for chat completion
#' (typically: "http://localhost:11434/api/chat")
#'
#' @return A new [llm_provider-class] object for use of the Ollama API
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

    for (name in names(self$parameters))
      body[[name]] <- self$parameters[[name]]

    request <- httr2::request(self$url) |>
      httr2::req_body_json(body)

    request_llm_provider(
      chat_history,
      request,
      stream = self$parameters$stream,
      verbose = self$verbose,
      api_type = self$api_type
    )
  }

  if (is.null(parameters$stream))
    parameters$stream <- FALSE

  ollama <- `llm_provider-class`$new(
    complete_chat_function = complete_chat,
    parameters = parameters,
    verbose = verbose,
    url = url,
    api_type = "ollama"
  )

  return(ollama)
}



#' Create a new OpenAI LLM provider
#'
#' This function creates a new [llm_provider-class] object that interacts with the Open AI API.
#'
#' @param parameters A named list of parameters. Currently the following parameters are required:
#'    - model: The name of the model to use
#'    - api_key: The API key to use for authentication with the OpenAI API. This
#'    should be a project API key (not a user API key)
#'    - url: The URL to the OpenAI API (may also be an alternative endpoint
#'    that provides a similar API.)
#'    - stream: A logical indicating whether the API should stream responses
#'
#'  Additional parameters are appended to the request body; see the OpenAI API
#'  documentation for more information: https://platform.openai.com/docs/api-reference/chat
#' @param verbose A logical indicating whether the interaction with the LLM provider
#' should be printed to the console. Default is TRUE.
#' @param url The URL to the OpenAI API endpoint for chat completion
#' (typically: "https://api.openai.com/v1/chat/completions")
#' @param api_key The API key to use for authentication with the OpenAI API
#'
#' @return A new [llm_provider-class] object for use of the OpenAI API
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

    body <- list(
      messages = lapply(seq_len(nrow(chat_history)), function(i) {
        list(role = chat_history$role[i], content = chat_history$content[i])
      })
    )

    for (name in names(self$parameters))
      body[[name]] <- self$parameters[[name]]

    request <- httr2::request(self$url) |>
      httr2::req_body_json(body) |>
      httr2::req_headers(!!!headers)

    request_llm_provider(
      chat_history,
      request,
      stream = self$parameters$stream,
      verbose = self$verbose,
      api_type = self$api_type
    )
  }

  return(`llm_provider-class`$new(
    complete_chat_function = complete_chat,
    parameters = parameters,
    verbose = verbose,
    url = url,
    api_key = api_key,
    api_type = "openai"
  ))
}



#' Create a new OpenRouter LLM provider
#'
#' This function creates a new [llm_provider-class] object that interacts with the OpenRouter API.
#'
#' @param parameters A named list of parameters. Currently the following parameters are required:
#'    - model: The name of the model to use
#'    - stream: A logical indicating whether the API should stream responses
#'
#'  Additional parameters are appended to the request body; see the OpenRouter API
#'  documentation for more information: https://openrouter.ai/docs/parameters
#' @param verbose A logical indicating whether the interaction with the LLM provider
#' should be printed to the console.
#' @param url The URL to the OpenRouter API endpoint for chat completion
#' @param api_key The API key to use for authentication with the OpenRouter API
#'
#' @return A new [llm_provider-class] object for use of the OpenRouter API
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



#' Create a new Mistral LLM provider
#'
#' This function creates a new [llm_provider-class] object that interacts with the Mistral API.
#'
#' @param parameters A named list of parameters. Currently the following parameters are required:
#'    - model: The name of the model to use
#'    - stream: A logical indicating whether the API should stream responses
#'
#'  Additional parameters are appended to the request body; see the Mistral API
#'  documentation for more information: https://docs.mistral.ai/api/#tag/chat
#' @param verbose A logical indicating whether the interaction with the LLM provider
#' should be printed to the consol
#' @param url The URL to the Mistral API endpoint for chat completion
#' @param api_key The API key to use for authentication with the Mistral API
#'
#' @return A new [llm_provider-class] object for use of the Mistral API
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



#' Create a new Groq LLM provider
#'
#' This function creates a new [llm_provider-class] object that interacts with the Groq API.
#'
#' @param parameters A named list of parameters. Currently the following parameters are required:
#'   - model: The name of the model to use
#'   - stream: A logical indicating whether the API should stream responses
#'
#'  Additional parameters are appended to the request body; see the Groq API
#'  documentation for more information: https://console.groq.com/docs/api-reference#chat-create
#' @param verbose A logical indicating whether the interaction with the LLM provider
#' should be printed to the console
#' @param api_key The API key to use for authentication with the Groq API
#' @param url The URL to the Groq API endpoint for chat completion
#'
#' @return A new [llm_provider-class] object for use of the Groq API
#'
#' @export
#' @example inst/examples/llm_providers.R
#'
#' @family llm_provider
llm_provider_groq <- function(
    parameters = list(
      model = "llama-3.1-8b-instant",
      stream = TRUE
    ),
    verbose = getOption("tidyprompt.verbose", TRUE),
    url = "https://api.groq.com/openai/v1/chat/completions",
    api_key = Sys.getenv("GROQ_API_KEY")
) {
  llm_provider_openai(parameters, verbose, url, api_key)
}



#' Create a new XAI (Grok) LLM provider
#'
#' This function creates a new [llm_provider-class] object that interacts with the XAI API.
#'
#' @param parameters A named list of parameters. Currently the following parameters are required:
#'   - model: The name of the model to use
#'   - stream: A logical indicating whether the API should stream responses
#'
#'  Additional parameters are appended to the request body; see the XAI API
#'  documentation for more information: https://docs.x.ai/api/endpoints#chat-completions
#' @param verbose A logical indicating whether the interaction with the LLM provider
#' should be printed to the console. Default is TRUE.
#' @param url The URL to the XAI API endpoint for chat completion
#' @param api_key The API key to use for authentication with the XAI API
#'
#' @return A new [llm_provider-class] object for use of the XAI API
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



#' Create a new Google Gemini LLM provider
#'
#' This function creates a new [llm_provider-class] object that interacts with the Google Gemini API.
#' Streaming is not yet supported in this implementation.
#'
#' @param parameters A named list of parameters. Currently the following parameters are required:
#'    - model: The name of the model to use (see: https://ai.google.dev/gemini-api/docs/models/gemini)
#'
#'  Additional parameters are appended to the request body; see the Google AI Studio API
#'  documentation for more information: https://ai.google.dev/gemini-api/docs/text-generation
#'  and https://github.com/google/generative-ai-docs/blob/main/site/en/gemini-api/docs/get-started/rest.ipynb
#' @param verbose A logical indicating whether the interaction with the LLM provider
#' should be printed to the console
#' @param url The URL to the Google Gemini API endpoint for chat completion
#' @param api_key The API key to use for authentication with the Google Gemini API
#' (see: https://aistudio.google.com/app/apikey)
#'
#' @return A new [llm_provider-class] object for use of the Google Gemini API
#'
#' @export
#'
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
    endpoint <- paste0(
      self$url,
      self$parameters$model,
      ":generateContent"
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

    # Send the POST request with httr2
    response <- httr2::request(endpoint) |>
      httr2::req_headers(
        `Content-Type` = "application/json",
        `Authorization` = paste("Bearer", self$api_key)
      ) |>
      httr2::req_body_json(body) |>
      httr2::req_url_query(key = self$api_key) |>
      httr2::req_perform()

    # Check if the request was successful
    if (httr2::resp_status(response) == 200) {
      content <- httr2::resp_body_json(response)

      return(list(
        role = "assistant",
        content = content$candidates[[1]]$content$parts[[1]]$text
      ))
    } else {
      stop("Error: ", httr2::resp_status(response), " - ", httr2::resp_body_string(response))
    }
  }

  `llm_provider-class`$new(
    complete_chat_function = complete_chat,
    parameters = parameters,
    verbose = verbose,
    url = url,
    api_key = api_key,
    api_type = "gemini"
  )
}



#' Create a fake [llm_provider-class] (for development and testing purposes)
#'
#' This function creates a fake [llm_provider-class] that can be used for development
#' and testing purposes. It is hardcoded to send back specific responses to
#' specific prompts that are used in vignettes, tests, and examples.
#' This is useful for running tests and builds in environments in which an
#' actual [llm_provider-class] is not available.
#'
#' @param verbose A logical indicating whether the interaction with the [llm_provider-class]
#' should be printed to the console. Default is TRUE.
#'
#' @return A new [llm_provider-class] object for use of the fake LLM provider
#'
#' @noRd
#' @keywords internal
llm_provider_fake <- function(verbose = getOption("tidyprompt.verbose", TRUE)) {
  complete_chat <- function(chat_history) {
    last_msg <- utils::tail(chat_history$content, 1)

    answer_as_integer_input <-
      "You must answer with only an integer (use no other characters)."

    chain_of_thought_input <-
      "To answer the user's prompt, you need to think step by step to arrive at a final answer."

    if (last_msg == "Hi there!") {
      return(list(completed = chat_history |> dplyr::bind_rows(data.frame(
        role = "assistant",
        content = paste0(
          "It's nice to meet you.",
          " Is there something I can help you with or would you like to chat?"
        )
      ))))
    }

    if (grepl(
      "What is a large language model? Explain in 10 words.", last_msg,
      fixed = TRUE
    )) {
      return(list(completed = chat_history |> dplyr::bind_rows(data.frame(
        role = "assistant",
        content = "Complex computer program trained on vast texts to generate human-like responses."
      ))))
    }

    if (
      grepl("What is 2 + 2?", last_msg, fixed = TRUE)
      & grepl(answer_as_integer_input, last_msg, fixed = TRUE)
      & !grepl(chain_of_thought_input, last_msg, fixed = TRUE)
    ) {
      return(list(completed = chat_history |> dplyr::bind_rows(data.frame(
        role = "assistant",
        content = "4"
      ))))
    }

    if (
      grepl("What is 2 + 2?", last_msg, fixed = TRUE)
      & !grepl(answer_as_integer_input, last_msg, fixed = TRUE)
    ) {
      return(list(completed = chat_history |> dplyr::bind_rows(data.frame(
        role = "assistant",
        content = glue::glue(
          ">> step 1: Identify the mathematical operation in the prompt,
          which is a simple addition problem.

          >> step 2: Recall the basic arithmetic fact that 2 + 2 equals a specific
          numerical value.

          >> step 3: Apply this knowledge to determine the result of the addition problem,
          using the known facts about numbers and their operations.

          >> step 4: Conclude that based on this mathematical understanding, the
          solution to the prompt \"What is 2 + 2?\" is a fixed numerical quantity."
        )
      ))))
    }

    if (
      any(grepl("What is 2 + 2?", chat_history$content[chat_history$role == "user"], fixed = TRUE))
      & grepl(answer_as_integer_input, last_msg, fixed = TRUE)
      & !grepl(chain_of_thought_input, last_msg, fixed = TRUE)
    ) {
      return(list(completed = chat_history |> dplyr::bind_rows(data.frame(
        role = "assistant",
        content = "22"
      ))))
    }

    if (
      grepl("What is 2 + 2?", last_msg,  fixed = TRUE)
      & grepl(chain_of_thought_input, last_msg,  fixed = TRUE)
      & grepl(answer_as_integer_input, last_msg,  fixed = TRUE)
    ) {
      return(list(completed = chat_history |> dplyr::bind_rows(data.frame(
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
      ))))
    }

    if (grepl(
      'example usage: FUNCTION[temperature_in_location]("Amsterdam", "Fahrenheit")',
      last_msg,
      fixed = TRUE
    )) {
      return(list(completed = chat_history |> dplyr::bind_rows(data.frame(
        role = "assistant",
        content = glue::glue(
          "I'll use the provided function to get the current temperature in Enschede.

          FUNCTION[temperature_in_location](\"Enschede\", \"Celcius\")"
        )
      ))))
    }

    if (
      grepl("function called: temperature_in_location", last_msg, fixed = TRUE)
      & grepl("arguments used: location = Enschede", last_msg, fixed = TRUE)
    ) {
      return(list(completed = chat_history |> dplyr::bind_rows(data.frame(
        role = "assistant",
        content = "22.7"
      ))))
    }

    if (
      any(grepl(
        "So the current temperature in Enschede is 22.7 degrees Celsius.",
        chat_history$content[chat_history$role == "assistant"],
        fixed = TRUE
      ))
      & grepl(last_msg, answer_as_integer_input, fixed = TRUE)
    ) {
      return(list(completed = chat_history |> dplyr::bind_rows(data.frame(
        role = "assistant",
        content = "22.7"
      ))))
    }

    return(list(completed = chat_history |> dplyr::bind_rows(data.frame(
      role = "assistant",
      content = "I'm a fake LLM! This is my default response."
    ))))
  }

  `llm_provider-class`$new(
    complete_chat_function = complete_chat,
    verbose = verbose,
    parameters = list(
      model = 'llama3.1:8b'
    ),
    api_type = "fake"
  )
}
