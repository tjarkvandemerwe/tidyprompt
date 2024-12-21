chat <- "Hi there!" |>
  chat_history()
chat

chat_from_df <- data.frame(
  role = c("user", "assistant"),
  content = c("Hi there!", "Hello! How can I help you today?")
) |>
  chat_history()
chat_from_df

# `add_msg_to_chat_history()` may be used to add messages to a chat history
chat_from_df <- chat_from_df |>
  add_msg_to_chat_history("Calculate 2+2 for me, please!")
chat_from_df

# You can also continue conversations which originate from `send_prompt()`:
\dontrun{
  result <- "Hi there!" |>
    send_prompt(return_mode = "full")
  # --- Sending request to LLM provider (llama3.1:8b): ---
  # Hi there!
  # --- Receiving response from LLM provider: ---
  # It's nice to meet you. Is there something I can help you with, or would you
  # like to chat?

  # Access the chat history from the result:
  chat_from_send_prompt <- result$chat_history

  # Add a message to the chat history:
  chat_history_with_new_message <- chat_from_send_prompt |>
    add_msg_to_chat_history("Let's chat!")

  # The new chat history can be input for a new tidyprompt:
  prompt <- tidyprompt(chat_history_with_new_message)

  # You can also take an existing tidyprompt and add the new chat history to it;
  #   this way, you can continue a conversation using the same prompt wraps
  prompt$set_chat_history(chat_history_with_new_message)

  # send_prompt() also accepts a chat history as input:
  new_result <- chat_history_with_new_message |>
    send_prompt(return_mode = "full")

  # You can also create a persistent chat history object from
  #   a chat history data frame; see ?`persistent_chat-class`
  chat <- `persistent_chat-class`$new(llm_provider_ollama(), chat_from_send_prompt)
  chat$chat("Let's chat!")
}
