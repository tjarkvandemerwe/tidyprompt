\dontrun{
  "What are a few capital cities around the world?" |>
    answer_as_key_value(
      key_name = "country",
      value_name = "capital"
    ) |>
    send_prompt()
  # --- Sending request to LLM provider (llama3.1:8b): ---
  # What are a few capital cities around the world?
  #
  # Respond with a list of key-value pairs, like so:
  #   -- <<country 1>>: <<capital 1>>
  #   -- <<country 2>>: <<capital 2>>
  #   etc.
  # --- Receiving response from LLM provider: ---
  # Here are a few:
  #   -- Australia: Canberra
  #   -- France: Paris
  #   -- United States: Washington D.C.
  #   -- Japan: Tokyo
  #   -- China: Beijing
  # $Australia
  # [1] "Canberra"
  #
  # $France
  # [1] "Paris"
  #
  # $`United States`
  # [1] "Washington D.C."
  #
  # $Japan
  # [1] "Tokyo"
  #
  # $China
  # [1] "Beijing"
}
