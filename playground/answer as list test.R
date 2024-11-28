"what are some delicious fruits?" |>
  answer_as_list(item_name = "fruit", n_unique_items = 10) |>
  send_prompt(llm_provider_openai())
