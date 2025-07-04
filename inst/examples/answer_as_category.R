\dontrun{
  "It is sunny, that makes me happy." |>
    answer_as_category(categories = c("environment", "weather", "work")) |>
    send_prompt()
  # --- Sending request to LLM provider (llama3.1:8b): ---
  #   You need to categorize a text.
  #
  # Text:
  #   'It is sunny, that makes me happy.'
  #
  # Possible categories:
  #   1. environment
  #   2. weather
  #   3. work
  #
  # Respond with the number of the category that best describes the text.
  # (Use no other words or characters.)
  # --- Receiving response from LLM provider: ---
  #   2
  # [1] "weather"
}
