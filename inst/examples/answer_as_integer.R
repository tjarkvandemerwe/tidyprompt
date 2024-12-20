\dontrun{
  "What is 5 + 5?" |>
    answer_as_integer() |>
    send_prompt()
  # --- Sending request to LLM provider (llama3.1:8b): ---
  #   What is 5 + 5?
  #
  #   You must answer with only an integer (use no other characters).
  # --- Receiving response from LLM provider: ---
  #   10
  # [1] 10
}
