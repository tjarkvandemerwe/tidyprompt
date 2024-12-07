tidyprompt("Hi!")

# Add to a tidyprompt using a prompt_wrap():
tidyprompt("Hi!") |>
  add_text("How are you?")

# Strings can be input for prompt wraps; therefore,
#   a call to tidyprompt() is not necessary:
"Hi" |>
  add_text("How are you?")

# Example of adding extraction & validation with a prompt_wrap():
prompt <- "Hi" |>
  add_text("What is 5 + 5?") |>
  answer_as_integer()

\dontrun{
  # tidyprompt objects are evaluated by send_prompt(), which will
  #   handle construct the prompt text, send it to the LLM provider,
  #   and apply the extraction and validation functions from the tidyprompt object
  prompt |>
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

# `tidyprompt` objects may be validated with these helpers:
is_tidyprompt(prompt) # Returns TRUE if input is a valid tidyprompt object

# Get base prompt text
get_base_prompt(prompt)

# Get all prompt wraps
get_prompt_wraps(prompt)

# Construct prompt text
construct_prompt_text(prompt)
