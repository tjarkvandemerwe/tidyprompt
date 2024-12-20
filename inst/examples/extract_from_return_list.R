\dontrun{
  response <- "Hi!" |>
    send_prompt(llm_provider_ollama(), return_mode = "full") |>
    extract_from_return_list("response")
  response
  # [1] "It's nice to meet you. Is there something I can help you with,
  # or would you like to chat?"
}
