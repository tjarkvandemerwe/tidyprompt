\dontrun{
  "What are some delicious fruits?" |>
    answer_as_list(item_name = "fruit", n_unique_items = 5) |>
    send_prompt(llm_provider_ollama())
#   --- Sending request to LLM provider (llama3.1:8b): ---
#     What are some delicious fruits?
#
#     Respond with a list, like so:
#       -- <<fruit 1>>
#       -- <<fruit 2>>
#       etc.
#     The list should contain 5 unique items.
#   --- Receiving response from LLM provider: ---
#     Here's a list of delicious fruits:
#
#     -- Strawberries
#     -- Pineapples
#     -- Mangoes
#     -- Papayas
#     -- Kiwis
# [1] "Strawberries" "Pineapples"   "Mangoes"      "Papayas"      "Kiwis"
}

