# Example creation of an OpenAI provider:
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
