"Please provide an example e-mail address" |>
  answer_as_regex("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$") |>
  send_prompt(llm_provider_openai())
