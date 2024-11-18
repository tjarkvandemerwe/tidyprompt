\dontrun{
  "Hi!" |>
    send_prompt(llm_provider_ollama())
  # --- Sending request to LLM provider (llama3.1:8b): ---
  #   Hi!
  # --- Receiving response from LLM provider: ---
  #   It's nice to meet you. Is there something I can help you with, or would you like to chat?
  # [1] "It's nice to meet you. Is there something I can help you with, or would you like to chat?"

  "Hi!" |>
    send_prompt(llm_provider_ollama(), return_mode = "full")
  # --- Sending request to LLM provider (llama3.1:8b): ---
  #   Hi!
  # --- Receiving response from LLM provider: ---
  #   It's nice to meet you. Is there something I can help you with, or would you like to chat?
  # $response
  # [1] "It's nice to meet you. Is there something I can help you with, or would you like to chat?"
  #
  # $chat_history
  # ...
  #
  # $chat_history_clean
  # ...
  #
  # $start_time
  # [1] "2024-11-18 15:43:12 CET"
  #
  # $end_time
  # [1] "2024-11-18 15:43:13 CET"
  #
  # $duration_seconds
  # [1] 1.13276
  #
  # $http_list
  # $http_list[[1]]
  # Response [http://localhost:11434/api/chat]
  #   Date: 2024-11-18 14:43
  #   Status: 200
  #   Content-Type: application/x-ndjson
  # <EMPTY BODY>

  "Hi!" |>
    add_text("What is 5 + 5?") |>
    answer_as_integer() |>
    send_prompt(llm_provider_ollama(), verbose = FALSE)
  # [1] 10
}
