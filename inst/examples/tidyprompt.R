tidyprompt("Hi!")

tidyprompt("Hi!") |>
  add_text("How are you?")

# Strings can be input for prompt wraps; therefore,
#   a call to tidyprompt() is often not necessary:
"Hi" |>
  add_text("How are you?")

\dontrun{
  # tidyprompt objects are evaluated by send_prompt(), which will
  #   handle construct the prompt text, send it to the LLM provider,
  #   and apply the extraction and validation functions from the tidyprompt object
  "Hi" |>
    add_text("What is 5 + 5?") |>
    answer_as_integer() |>
    send_prompt(llm_provider_ollama())
  # --- Sending request to LLM provider (llama3.1:8b): ---
  #   Hi
  #
  #   What is 5 + 5?
  #
  #   You must answer with only an integer (use no other characters).
  # --- Receiving response from LLM provider: ---
  #   10
  # [1] 10

  # See prompt_wrap() and send_prompt() for more details
}
