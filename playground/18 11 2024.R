"Hi" |>
  add_text("What is my name?") |>
  quit_if() |>
  send_prompt(llm_provider_ollama())
