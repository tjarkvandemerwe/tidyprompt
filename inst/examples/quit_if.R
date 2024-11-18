\dontrun{
  "What the favourite food of my cat on Thursday mornings?" |>
    quit_if() |>
    send_prompt(llm_provider_ollama())
  # --- Sending request to LLM provider (llama3.1:8b): ---
  #   What the favourite food of my cat on Thursday mornings?
  #
  #   If you think that you cannot provide a valid answer, you must type:
  #   'NO ANSWER' (use no other characters)
  # --- Receiving response from LLM provider: ---
  #   NO ANSWER
  # NULL
}


