prompt <- "Hi there!" |>
  add_text("How is your day?")
prompt
prompt |>
  construct_prompt_text()
