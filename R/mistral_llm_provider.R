#' Create a new Mistral llm_provider instance
#'
#' This function creates a new llm_provider that interacts with the Mistral API.
#'
#' @param parameters A named list of parameters. Currently the following parameters are required:
#'  - model: The name of the model to use (e.g., "mistral-small-latest")
#'  - api_key: The API key to use for authentication with the Mistral API. Store the API key safely.
#'  - temperature, top_p, max_tokens, stream, stop, random_seed, tool_choice, safe_prompt:
#'    Additional options to control the model's behavior.
#'
#' @return A new llm_provider object for use of the Mistral API
#'
#' @export
create_mistral_llm_provider <- function(parameters = list(
  model = "mistral-small-latest",
  api_key = Sys.getenv("MISTRAL_API_KEY"),
  temperature = 0.3,
  top_p = 1,
  max_tokens = 0,
  stream = FALSE,
  stop = NULL,
  random_seed = 0,
  tool_choice = "auto",
  safe_prompt = FALSE
)) {
  complete_chat <- function(chat_history) {
    url <- "https://api.mistral.ai/v1/chat/completions"
    headers <- c(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", parameters$api_key)
    )

    # Prepare the body by converting chat_history dataframe to list of lists
    body <- list(
      model = parameters$model,
      temperature = parameters$temperature,
      top_p = parameters$top_p,
      max_tokens = parameters$max_tokens,
      stream = parameters$stream,
      stop = parameters$stop,
      random_seed = parameters$random_seed,
      messages = lapply(seq_len(nrow(chat_history)), function(i) {
        list(role = chat_history$role[i], content = chat_history$content[i])
      }),
      response_format = list(type = "text"),
      tool_choice = parameters$tool_choice,
      safe_prompt = parameters$safe_prompt
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
