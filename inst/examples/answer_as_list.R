\dontrun{
  "What are some delicious fruits?" |>
    answer_as_list(item_name = "fruit", n_unique_items = 5) |>
    send_prompt()
  # --- Sending request to LLM provider (llama3.1:8b): ---
  # What are some delicious fruits?
  #
  # Respond with a list, like so:
  #   -- <<fruit 1>>
  #   -- <<fruit 2>>
  #   etc.
  # The list should contain 5 unique items.
  # --- Receiving response from LLM provider: ---
  # Here's the list of delicious fruits:
  #   -- Strawberries
  #   -- Pineapples
  #   -- Mangoes
  #   -- Blueberries
  #   -- Pomegranates
  # [[1]]
  # [1] "Strawberries"
  #
  # [[2]]
  # [1] "Pineapples"
  #
  # [[3]]
  # [1] "Mangoes"
  #
  # [[4]]
  # [1] "Blueberries"
  #
  # [[5]]
  # [1] "Pomegranates"
}

