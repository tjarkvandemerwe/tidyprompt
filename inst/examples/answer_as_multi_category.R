\dontrun{
  "It is sunny, that makes me happy." |>
    answer_as_multi_category(
      categories = c("environment", "weather", "work", "positive", "negative")
    ) |>
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
  #   4. positive
  #   5. negative
  #
  # Respond with the numbers of all categories that apply to this text, separated by commas.
  # (Use no other words or characters.)
  # --- Receiving response from LLM provider: ---
  #   2, 4
  # ["weather","positive"]
}
