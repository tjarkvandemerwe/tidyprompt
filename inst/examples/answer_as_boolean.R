\dontrun{
  "Are you a large language model?" |>
    answer_as_boolean() |>
    send_prompt()
  # --- Sending request to LLM provider (llama3.1:8b): ---
  #   Are you a large language model?
  #
  #   You must answer with only TRUE or FALSE (use no other characters).
  # --- Receiving response from LLM provider: ---
  #   TRUE
  # [1] TRUE
}
