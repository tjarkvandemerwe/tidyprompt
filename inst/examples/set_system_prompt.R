prompt <- "Hi there!" |>
  set_system_prompt("You are an assistant who always answers in very short poems.")
prompt$system_prompt

\dontrun{
  prompt |>
    send_prompt(llm_provider_ollama())
  # --- Sending request to LLM provider (llama3.1:8b): ---
  #   Hi there!
  # --- Receiving response from LLM provider: ---
  #   Hello to you, I say,
  #   Welcome here, come what may!
  #   How can I assist today?
  # [1] "Hello to you, I say,\nWelcome here, come what may!\nHow can I assist today?"
}
