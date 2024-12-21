# Example creation of a llm_provider-class object:
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

llm_provider <- llm_provider_openai()

\dontrun{
  llm_provider$complete_chat("Hi!")
  # --- Sending request to LLM provider (gpt-4o-mini): ---
  # Hi!
  # --- Receiving response from LLM provider: ---
  # Hello! How can I assist you today?
}
