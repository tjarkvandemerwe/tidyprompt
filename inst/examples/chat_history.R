chat <- "Hi there!" |>
  chat_history()
chat

chat_from_df <- data.frame(
  role = c("user", "assistant"),
  content = c("Hi there!", "Hello! How can I help you today?")
) |>
  chat_history()
chat_from_df

# `chat_history_add_msg()` may be used to add messages to a chat history
chat_from_df <- chat_from_df |>
  chat_history_add_msg("Calculate 2+2 for me, please!")
chat_from_df

# You can also call `chat_history_add_msg()` on a list
#   containing '$chat_history`;
#   e.g., a result of `send-prompt()` with `return_mode = "full"`
#     This may be used to continue a chat history in a new session
\dontrun{
  result <- "Hi there!" |>
    send_prompt(return_mode = "full")
  result$response
}
