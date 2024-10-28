#' Function to create llm_provider objects
#'
#' This function can be used to create new llm_provider objects with different
#' implementations of the complete_chat function.
#'
#' @param complete_chat_function Function that will be called by the llm_provider
#' to complete a chat.
#'
#' This function should take a chat_history dataframe as input (see ?chat_history),
#' and return a list of 'role' and 'content' for the next message (e.g., list(role = "user", content = "Hello")).
#'
#' An llm_provider object will wrap the provided complete_chat_function with a validation of chat_history,
#' also turning a single string into a valid chat_history dataframe.
#' The provided function thus does not need to do this but should assume that
#' the input is a valid chat_history dataframe.
#'
#' Parameters passed in the parameters argument may be accessed by the complete_chat_function;
#' this may be used to, for instance, store an API key, the name of the model to use, or other settings.
#'
#' @param parameters A named list of parameters that will be attached to the
#'  llm_provider object. These parameters can be used to configure the llm_provider.
#'  For instance, they can be used to store a model's name, an API key, or an endpoint.
#'  E.g., list(model = "my-llm-model", api_key = "my-api-key").
#' @param verbose A logical indicating whether the interaction with the LLM provider
#' should be printed to the console. Default is TRUE.
#'
#' @return A new llm_provider object
#'
#' @export
llm_provider <- function(
    complete_chat_function,
    parameters = list(),
    verbose = getOption("tidyprompt.verbose", TRUE)
) {
  if (length(parameters) > 0 & is.null(names(parameters))) {
    stop("parameters must be a named list")
  }

  # Create llm_provider structure
  llm_provider <- structure(
    list(),
    class = "llm_provider"
  )

  # Create a new environment for the llm_provider
  llm_provider_env <- new.env()

  # Store initial parameters in the environment
  llm_provider_env$parameters <- parameters

  # Helper function to create functions within llm_provider_env
  create_function <- function(fn) {
    environment(fn) <- llm_provider_env
    return(fn)
  }

  # Access parameters
  llm_provider$get_parameters <- create_function(function() {
    return(llm_provider_env$parameters)
  })

  # Update parameters
  llm_provider$set_parameters <- create_function(function(new_parameters) {
    if (length(new_parameters) > 0 & is.null(names(new_parameters))) {
      stop("new_parameters must be a named list")
    }
    # Update environment parameters directly
    llm_provider_env$parameters <- utils::modifyList(llm_provider_env$parameters, new_parameters)
  })

  # Get environment
  llm_provider$get_env <- create_function(function() {
    return(llm_provider_env)
  })

  # Attach complete_chat with chat_history validation
  llm_provider$complete_chat <- create_function(function(chat_history) {
    chat_history <- chat_history(chat_history)
    if (verbose) {
      message("--- Sending message to LLM provider: ---")
      cat(chat_history$content[nrow(chat_history)])
      cat("\n")
    }

    environment(complete_chat_function) <- llm_provider_env

    if (verbose)
      message("--- Receiving response from LLM provider: ---")

    response <- complete_chat_function(chat_history)

    if (verbose & isTRUE(parameters$stream == FALSE)) {
      message(response$content)
    }

    return(response)
  })

  # Get verbose setting
  llm_provider$get_verbose <- create_function(function() {
    return(verbose)
  })
  # Set verbose setting
  llm_provider$set_verbose <- create_function(function(new_verbose) {
    if (!is.logical(new_verbose)) {
      stop("new_verbose must be a logical")
    }
    verbose <<- new_verbose
  })

  # Return the llm_provider object
  return(llm_provider)
}



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
#' @param api_type The type of API to use; specifically required to handle streaming.
#' Currently, "openai" and "ollama" have been implemented. "openai" should also work
#' with other similar APIs for chat completion. If your API handles streaming in a
#' different way, you may need to implement your own version of this function
#' (you are then encouraged to submit a pull request to the GitHub repo of 'tidyprompt').
#'
#' @return A list with the role and content of the response from the LLM provider
#' @export
make_llm_provider_request <- function(
    url, headers = NULL, body, stream, verbose = getOption("tidyprompt.verbose", TRUE),
    api_type = c("openai", "ollama")
) {
  if (stream) {
    api_type <- match.arg(api_type)

    role <- NULL
    message <- ""

    write_stream_function <- function(x) {
      if (api_type == "ollama") {
        content <- x |> rawToChar() |> jsonlite::fromJSON()

        if (verbose)
          cat(content$message$content)

        if (is.null(role))
          role <<- content$message$role

        message <<- paste0(message, content$message$content)
      }

      if (api_type == "openai") {
        char <- x |>
          rawToChar() |>
          strsplit(split = "\ndata: ") |>
          unlist()

        char <- char[char != "[DONE]\n\n"]
        char <- char[char != ""]

        parsed_data <- char |>
          lapply(\(chunk) fromJSON(sub("^data:\\s*", "", chunk)))

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
  if (!stream) {
    content <- httr::content(response, as = "parsed")
    role <- content$message$role
    message <- content$message$content
  }

  return(list(
    role = role,
    content = message
  ))
}




#' Create a new Ollama llm_provider instance
#'
#' @param parameters A named list of parameters. Currently the following parameters are required:
#'    - model: The name of the model to use (e.g., "llama3.1:8b")
#'    - url: The URL of the Ollama API endpoint
#'    - stream: A logical indicating whether the API should stream responses (default: TRUE)
#'  Additional parameters may be passed by adding them to the parameters list;
#'  these parameters will be passed to the Ollama API via the body of the POST request.
#'  Options specifically can be set with the $set_options function (e.g.,
#'  ollama$set_options(list(temperature = 0.8))). See available options at
#'  https://ollama.com/docs/api/chat.#'
#' @param verbose A logical indicating whether the interaction with the LLM provider
#' should be printed to the console. Default is TRUE.
#'
#' @return A new llm_provider object for use of the Ollama API
#' @export
llm_provider_ollama <- function(parameters = list(
  model = "llama3.1:8b",
  url = "http://localhost:11434/api/chat",
  stream = TRUE
), verbose = getOption("tidyprompt.verbose", TRUE)) {
  complete_chat <- function(chat_history) {
    url <- parameters$url

    body <- list(
      model = parameters$model,
      messages = lapply(seq_len(nrow(chat_history)), function(i) {
        list(role = chat_history$role[i], content = chat_history$content[i])
      }),
      stream = parameters$stream
    )

    # Append all other parameters to the body
    for (name in names(parameters)) {
      if (!(name %in% c("model", "url", "stream"))) {
        body[[name]] <- parameters[[name]]
      }
    }

    return(make_llm_provider_request(
      url = url,
      headers = NULL,
      body = body,
      stream = parameters$stream,
      verbose = verbose,
      api_type = "ollama"
    ))
  }

  ollama <- llm_provider(
    complete_chat_function = complete_chat,
    parameters = parameters,
    verbose = verbose
  )

  # Additional functions to get/set options for the Ollama API
  set_options <- function(named_list_of_options) {
    if (length(named_list_of_options) == 0)
      stop("No options provided")

    if (length(named_list_of_options) > 0 & is.null(names(named_list_of_options))) {
      stop("named_list_of_options must be a named list")
    }

    # Merge new options with existing ones
    options <- parameters$options
    if (length(options) == 0)
      options <- list()

    updated_options <- utils::modifyList(options, named_list_of_options)
    for (name in names(updated_options)) {
      assign(name, updated_options[[name]], envir = environment())
    }
    parameters$options <<- updated_options
  }
  environment(set_options) <- ollama$get_env()
  ollama$set_options <- set_options

  # Function to get the options
  get_options <- function() {
    return(parameters$options)
  }
  environment(get_options) <- ollama$get_env()
  ollama$get_options <- get_options

  return(ollama)
}



#' Create a new OpenAI llm_provider instance
#'
#' This function creates a new llm_provider that interacts with the Open AI API
#'
#' @param parameters A named list of parameters. Currently the following parameters are required:
#'    - model: The name of the model to use (e.g., "gpt-4o-mini")
#'    - api_key: The API key to use for authentication with the OpenAI API. This should be
#'      a project API key (not a user API key) and it should have sufficient permissions.
#'    - url: The URL to the OpenAI API (default: "https://api.openai.com/v1/chat/completions").
#'      (May also be an alternative endpoint that provides a similar API.)
#'    - stream: A logical indicating whether the API should stream responses (default: TRUE)
#'  Additional parameters are appended to the request body; see the OpenAI API
#'  documentation for more information: https://platform.openai.com/docs/api-reference/chat
#' @param verbose A logical indicating whether the interaction with the LLM provider
#' should be printed to the console. Default is TRUE.
#'
#' @return A new llm_provider object for use of the OpenAI API
#'
#' @export
llm_provider_openai <- function(parameters = list(
  model = "gpt-4o-mini",
  api_key = Sys.getenv("OPENAI_API_KEY"),
  url = "https://api.openai.com/v1/chat/completions",
  stream = TRUE
), verbose = getOption("tidyprompt.verbose", TRUE)) {
  complete_chat <- function(chat_history) {
    url <- parameters$url
    headers <- c(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", parameters$api_key)
    )

    # Prepare the body by converting chat_history dataframe to list of lists
    body <- list(
      messages = lapply(seq_len(nrow(chat_history)), function(i) {
        list(role = chat_history$role[i], content = chat_history$content[i])
      })
    )

    # Add all other parameters to the body
    body <- c(body, parameters[names(parameters) != "api_key" & names(parameters) != "url"])

    make_llm_provider_request(
      url = url,
      headers = headers,
      body = body,
      stream = parameters$stream,
      verbose = verbose,
      api_type = "openai"
    )
  }

  llm_provider(
    complete_chat_function = complete_chat,
    parameters = parameters,
    verbose = verbose
  )
}



#' Create a new OpenRouter llm_provider instance
#'
#' @param parameters A named list of parameters. Currently the following parameters are required:
#'  - model: The name of the model to use (e.g., "qwen/qwen-2.5-7b-instruct"; see
#'  https://openrouter.ai/docs/models).
#'  - api_key: The API key to use for authentication with the OpenRouter API (see
#'  https://openrouter.ai/docs/api-keys).
#'  - url: The URL to the OpenRouter API (default: "https://openrouter.ai/api/v1/chat/completions").
#'  Additional parameters are appended to the request body; see the OpenRouter API
#'  documentation for more information: https://openrouter.ai/docs/parameters
#' @param verbose A logical indicating whether the interaction with the LLM provider
#' should be printed to the console. Default is TRUE.
#'
#' @return A new llm_provider object for use of the OpenRouter API
#' @export
llm_provider_openrouter <- function(parameters = list(
  model = "qwen/qwen-2.5-7b-instruct",
  api_key = Sys.getenv("OPENROUTER_API_KEY"),
  url = "https://openrouter.ai/api/v1/chat/completions"
), verbose = getOption("tidyprompt.verbose", TRUE)) {
  # OpenRouter follows the same API structure as OpenAI
  llm_provider_openai(parameters, verbose)
}



#' Create a new Mistral llm_provider instance
#'
#' This function creates a new llm_provider that interacts with the Mistral API.
#'
#' @param parameters A named list of parameters. Currently the following parameters are required:
#'    - model: The name of the model to use (e.g., "mistral-small-latest")
#'    - api_key: The API key to use for authentication with the Mistral API
#'    - url: The URL to the Mistral API (default: "https://api.mistral.ai/v1/chat/completions")
#'  Additional parameters are appended to the request body; see the Mistral API
#'  documentation for more information: https://docs.mistral.ai/api/#tag/chat
#' @param verbose A logical indicating whether the interaction with the LLM provider
#' should be printed to the console. Default is TRUE.
#'
#' @return A new llm_provider object for use of the Mistral API
#'
#' @export
llm_provider_mistral <- function(parameters = list(
  model = "mistral-small-latest",
  api_key = Sys.getenv("MISTRAL_API_KEY"),
  url = "https://api.mistral.ai/v1/chat/completions"
), verbose = getOption("tidyprompt.verbose", TRUE)) {
  # Mistral follows the same API structure as OpenAI
  llm_provider_openai(parameters, verbose)
}



#' Create a new Groq llm_provider instance
#'
#' @param parameters A named list of parameters. Currently the following parameters are required:
#'   - model: The name of the model to use (e.g., "llama-3.1-8b-instant")
#'   - api_key: The API key to use for authentication with the Groq API
#'   - url: The URL to the Groq API (default: "https://api.groq.com/openai/v1/chat/completions")
#'  Additional parameters are appended to the request body; see the Groq API
#'  documentation for more information: https://console.groq.com/docs/api-reference#chat-create
#' @param verbose A logical indicating whether the interaction with the LLM provider
#' should be printed to the console. Default is TRUE.
#'
#' @return A new llm_provider object for use of the Groq API
#' @export
llm_provider_groq <- function(parameters = list(
  model = "llama-3.1-8b-instant",
  api_key = Sys.getenv("GROQ_API_KEY"),
  url = "https://api.groq.com/openai/v1/chat/completions"
), verbose = getOption("tidyprompt.verbose", TRUE)) {
  # Groq follows the same API structure as OpenAI
  llm_provider_openai(parameters, verbose)
}



#' Create a new XAI (Grok) llm_provider instance
#'
#' @param parameters A named list of parameters. Currently the following parameters are required:
#'   - model: The name of the model to use (e.g., "grok-beta")
#'   - api_key: The API key to use for authentication with the XAI API
#'   - url: The URL to the XAI API (default: "https://api.xai.ai/v1/chat/completions")
#'  Additional parameters are appended to the request body; see the XAI API
#'  documentation for more information: https://docs.x.ai/api/endpoints#chat-completions
#' @param verbose A logical indicating whether the interaction with the LLM provider
#' should be printed to the console. Default is TRUE.
#'
#' @return A new llm_provider object for use of the XAI API
#' @export
llm_provider_xai <- function(parameters = list(
  model = "grok-beta",
  api_key = Sys.getenv("XAI_API_KEY"),
  url = "https://api.x.ai/v1/chat/completions"
), verbose = getOption("tidyprompt.verbose", TRUE)) {
  # XAI follows the same API structure as OpenAI
  llm_provider_openai(parameters, verbose)
}



#' Create a new Google Gemini llm_provider instance
#'
#' Creates an llm_provider object that interacts with the Google Gemini API.
#' Streaming is not yet supported in this implementation.
#'
#' @param parameters A named list of parameters. Currently the following parameters are required:
#'  - model: The name of the model to use (e.g., "gemini-1.5-flash"; see https://ai.google.dev/gemini-api/docs/models/gemini)
#'  - api_key: The API key to use for authentication (get at https://aistudio.google.com/app/apikey)
#'  - base_url: The URL to the Google AI Studio API (default: "https://generativelanguage.googleapis.com/v1beta/models/")
#'  Additional parameters are appended to the request body; see the Google AI Studio API
#'  documentation for more information: https://ai.google.dev/gemini-api/docs/text-generation
#'  and https://github.com/google/generative-ai-docs/blob/main/site/en/gemini-api/docs/get-started/rest.ipynb
#' @param verbose A logical indicating whether the interaction with the LLM provider
#' should be printed to the console. Default is TRUE.
#'
#' @return A new llm_provider object for use of the Google Gemini API
#' @export
llm_provider_google_gemini <- function(parameters = list(
  model = "gemini-1.5-flash",
  api_key = Sys.getenv("GOOGLE_AI_STUDIO_API_KEY"),
  base_url = "https://generativelanguage.googleapis.com/v1beta/models/"
), verbose = getOption("tidyprompt.verbose", TRUE)) {
  complete_chat <- function(chat_history) {
    # Construct URL for the API request
    url <- paste0(
      parameters$base_url,
      parameters$model,
      ":generateContent",
      "?key=", parameters$api_key
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

    # Add aditional parameters to the body
    body <- c(body,
      parameters[
        names(parameters) != "api_key"
        & names(parameters) != "base_url"
        & names(parameters) != "model"
      ]
    )

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

  llm_provider(
    complete_chat_function = complete_chat,
    parameters = parameters,
    verbose = verbose
  )
}



#' Create a fake LLM provider (for development and testing purposes)
#'
#' This function creates a fake LLM provider that can be used for development
#' and testing purposes. It is hardcoded to send back specific responses to
#' specific prompts that are used in vignettes, tests, and examples.
#' This is useful for running tests and builds in environments in which an
#' actual LLM provider is not available.
#'
#' @param verbose A logical indicating whether the interaction with the LLM provider
#' should be printed to the console. Default is TRUE.
#'
#' @return A new llm_provider object for use of the fake LLM provider
#' @export
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

  llm_provider(
    complete_chat_function = complete_chat,
    verbose = verbose
  )
}
