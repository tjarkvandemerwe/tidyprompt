\dontrun{
  "What is a large language model?" |>
    answer_as_text(max_words = 10) |>
    send_prompt()
  # --- Sending request to LLM provider (llama3.1:8b): ---
  # What is a large language model?
  #
  # You must provide a text response. The response must be at most 10 words.
  # --- Receiving response from LLM provider: ---
  # A type of AI that processes and generates human-like text.
  # [1] "A type of AI that processes and generates human-like text."
}
