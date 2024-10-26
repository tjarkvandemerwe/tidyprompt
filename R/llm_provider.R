#' Generic function to assist in creating llm_provider objects
#'
#' This function can be used to create new llm_provider objects with different
#' implementations of the complete_chat function.
#'
#' @param complete_chat_function Function that will be called by the llm_provider
#' to complete a chat.
#'
#' This function should take a chat_history dataframe as input (see ?validate_chat_history),
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
#'
#' @return A new llm_provider object
#'
#' @export
create_llm_provider <- function(
    complete_chat_function, parameters = list()
) {
  llm_provider <- structure(
    list(),
    class = "llm_provider"
  )

  # Create a new environment for the llm_provider
  llm_provider_env <- new.env()
  if (length(parameters) > 0 & is.null(names(parameters))) {
    stop("parameters must be a named list")
  }
  # Assign the parameters to the environment
  for (name in names(parameters)) {
    assign(name, parameters[[name]], envir = llm_provider_env)
  }

  # Helper function to create functions with the shared environment
  create_function <- function(fn) {
    environment(fn) <- llm_provider_env
    return(fn)
  }

  llm_provider$get_env <- create_function(function() {
    return(llm_provider_env)
  })

  # Get parameters attached this llm_provider
  llm_provider$get_parameters <- create_function(function() {
    return(parameters)
  })

  # Set parameters attached this llm_provider
  llm_provider$set_parameters <- create_function(function(new_parameters) {
    if (length(new_parameters) > 0 & is.null(names(new_parameters))) {
      stop("new_parameters must be a named list")
    }
    # Merge new parameters with existing ones
    updated_parameters <- utils::modifyList(parameters, new_parameters)
    for (name in names(updated_parameters)) {
      assign(name, updated_parameters[[name]], envir = llm_provider_env)
    }
    parameters <<- updated_parameters
  })

  # Install the provided complete_chat for this llm_provider;
  #   wrap some validation of chat_history around it
  llm_provider$complete_chat <- create_function(function(chat_history) {
    chat_history <- validate_chat_history(chat_history)
    environment(complete_chat_function) <- llm_provider_env

    # Call original function
    complete_chat_function(chat_history)
  })

  return(llm_provider)
}



#' Create a new Ollama llm_provider instance
#'
#' @param parameters A named list of parameters. Currently the following parameters are required:
#' - model: The name of the model to use (e.g., "llama3.1:8b")
#' - url: The URL of the Ollama API endpoint

#' Additional parameters may be passed by adding them to the parameters list;
#' these parameters will be passed to the Ollama API via the body of the POST request.
#' Options specifically can be set with the $set_option function (e.g.,
#' ollama$set_option("option_name", "option_value")). See options at
#' https://ollama.com/docs/api/chat.
#'
#' @return A new llm_provider object for use of the Ollama API
#' @export
create_ollama_llm_provider <- function(parameters = list(
  model = "llama3.1:8b",
  url = "http://localhost:11434/api/chat"
)) {
  complete_chat <- function(chat_history) {
    url <- parameters$url

    body <- list(
      model = parameters$model,
      messages = lapply(seq_len(nrow(chat_history)), function(i) {
        list(role = chat_history$role[i], content = chat_history$content[i])
      }),
      stream = FALSE
    )

    # Append all other parameters to the body
    for (name in names(parameters)) {
      if (!(name %in% c("model", "url"))) {
        body[[name]] <- parameters[[name]]
      }
    }

    # Make the POST request
    response <- httr::POST(url, body = body, encode = "json")

    # Check if the request was successful
    if (httr::status_code(response) == 200) {
      content <- httr::content(response, as = "parsed")
      return(list(
        role = content$message$role,
        content = content$message$content
      ))
    } else {
      stop("Error: ", httr::status_code(response), " - ", httr::content(response, as = "text"))
    }
  }

  ollama <- create_llm_provider(
    complete_chat_function = complete_chat,
    parameters = parameters
  )

  # Additional functions to get/set options for the Ollama API
  #   These options are a list within the regular parameters list
  #   While they can be set directly within the parameters list,
  #   these functions provide a more convenient way to manage them
  #   (given their position in the body of the POST request sent to Ollama)
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
  # Connect function to the provider environment:
  environment(set_options) <- ollama$get_env()
  # Add the function to the provider object:
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
#'  Additional parameters are appended to the request body; see the OpenAI API
#'  documentation for more information: https://platform.openai.com/docs/api-reference/chat
#'
#' @return A new llm_provider object for use of the OpenAI API
#'
#' @export
create_openai_llm_provider <- function(parameters = list(
  model = "gpt-4o-mini",
  api_key = Sys.getenv("OPENAI_API_KEY"),
  url = "https://api.openai.com/v1/chat/completions"
)) {
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

    response <- httr::POST(url, httr::add_headers(.headers = headers), body = body, encode = "json")

    # Check if the request was successful
    if (httr::status_code(response) == 200) {
      content <- httr::content(response, as = "parsed")
      return(list(
        role = content$choices[[1]]$message$role,
        content = content$choices[[1]]$message$content
      ))
    } else {
      stop("Error: ", httr::status_code(response))
    }
  }

  create_llm_provider(
    complete_chat_function = complete_chat,
    parameters = parameters
  )
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
#'
#' @return A new llm_provider object for use of the Mistral API
#'
#' @export
create_mistral_llm_provider <- function(parameters = list(
  model = "mistral-small-latest",
  api_key = Sys.getenv("MISTRAL_API_KEY"),
  url = "https://api.mistral.ai/v1/chat/completions"
)) {
  # Mistral follows the same API structure as OpenAI
  create_openai_llm_provider(parameters)
}



#' Create a new Groq llm_provider instance
#'
#' @param parameters A named list of parameters. Currently the following parameters are required:
#'   - model: The name of the model to use (e.g., "llama-3.1-8b-instant")
#'   - api_key: The API key to use for authentication with the Groq API
#'   - url: The URL to the Groq API (default: "https://api.groq.com/openai/v1/chat/completions")
#'  Additional parameters are appended to the request body; see the Groq API
#'  documentation for more information: https://console.groq.com/docs/api-reference#chat-create
#'
#' @return A new llm_provider object for use of the Groq API
#' @export
create_groq_llm_provider <- function(parameters = list(
  model = "llama-3.1-8b-instant",
  api_key = Sys.getenv("GROQ_API_KEY"),
  url = "https://api.groq.com/openai/v1/chat/completions"
)) {
  # Groq follows the same API structure as OpenAI
  create_openai_llm_provider(parameters)
}



#' Create a new XAI (Grok) llm_provider instance
#'
#' @param parameters A named list of parameters. Currently the following parameters are required:
#'   - model: The name of the model to use (e.g., "grok-beta")
#'   - api_key: The API key to use for authentication with the XAI API
#'   - url: The URL to the XAI API (default: "https://api.xai.ai/v1/chat/completions")
#'  Additional parameters are appended to the request body; see the XAI API
#'  documentation for more information: https://docs.x.ai/api/endpoints#chat-completions
#'
#' @return A new llm_provider object for use of the XAI API
#' @export
create_xai_llm_provider <- function(parameters = list(
  model = "grok-beta",
  api_key = Sys.getenv("XAI_API_KEY"),
  url = "https://api.x.ai/v1/chat/completions"
)) {
  # XAI follows the same API structure as OpenAI
  create_openai_llm_provider(parameters)
}


#' Create a new Google Gemini llm_provider instance
#'
#' @param parameters A named list of parameters. Currently the following parameters are required:
#'  - model: The name of the model to use (e.g., "gemini-1.5-flash")
#'  - api_key: The API key to use for authentication with the Google AI Studio API
#'  - base_url: The URL to the Google AI Studio API (default: "https://generativelanguage.googleapis.com/v1beta/models/")
#'  Additional parameters are appended to the request body; see the Google AI Studio API
#'  documentation for more information: https://ai.google.dev/gemini-api/docs/text-generation
#'  and https://github.com/google/generative-ai-docs/blob/main/site/en/gemini-api/docs/get-started/rest.ipynb
#'
#' @return A new llm_provider object for use of the Google Gemini API
#' @export
create_google_gemini_llm_provider <- function(parameters = list(
  model = "gemini-1.5-flash",
  api_key = Sys.getenv("GOOGLE_AI_STUDIO_API_KEY"),
  base_url = "https://generativelanguage.googleapis.com/v1beta/models/"
)) {
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

  create_llm_provider(
    complete_chat_function = complete_chat,
    parameters = parameters
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
#' @return A new llm_provider object for use of the fake LLM provider
#' @export
create_fake_llm_provider <- function() {
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

  create_llm_provider(
    complete_chat_function = complete_chat
  )
}
