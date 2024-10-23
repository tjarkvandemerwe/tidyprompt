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

    # Call original function
    complete_chat_function(chat_history)
  })

  return(llm_provider)
}

#' Create a new OpenAI llm_provider instance
#'
#' This function creates a new llm_provider that interacts with the Open AI API
#'
#' @param parameters A named list of parameters. Currently the following parameters are required:
#'  - model: The name of the model to use (e.g., "gpt-4o-mini")
#'  - api_key: The API key to use for authentication with the OpenAI API. This should be
#'  a project API key (not a user API key) and it should have sufficient permissions.
#'  It is recommended to safely store the API key in an environment variable.
#'
#' @return A new llm_provider object for use of the OpenAI API
#'
#' @export
create_openai_llm_provider <- function(parameters = list(
  model = "gpt-4o-mini",
  api_key = Sys.getenv("OPENAI_API_KEY")
)) {
  complete_chat <- function(chat_history) {
    url <- "https://api.openai.com/v1/chat/completions"
    headers <- c(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", parameters$api_key)
    )

    # Prepare the body by converting chat_history dataframe to list of lists
    body <- list(
      model = parameters$model,
      messages = lapply(seq_len(nrow(chat_history)), function(i) {
        list(role = chat_history$role[i], content = chat_history$content[i])
      })
    )

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

#' Create a new Ollama llm_provider instance
#'
#' @param parameters A named list of parameters. Currently the following parameters are required:
#' - model: The name of the model to use (e.g., "llama3.1:8b")
#' - url: The URL of the Ollama API endpoint
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

  create_llm_provider(
    complete_chat_function = complete_chat,
    parameters = parameters
  )
}
