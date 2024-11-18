\dontrun{
  "Hi!" |>
    send_prompt(llm_provider_ollama(), return_mode = "full") |>
    extract_from_return_list("response")
  # --- Sending request to LLM provider (llama3.1:8b): ---
  #   Hi!
  # --- Receiving response from LLM provider: ---
  #   It's nice to meet you. Is there something I can help you with or would you like to chat?
  # [1] "It's nice to meet you. Is there something I can help you with or would you like to chat?"
}
