% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chat_history.R
\name{add_msg_to_chat_history}
\alias{add_msg_to_chat_history}
\title{Add a message to a chat history}
\usage{
add_msg_to_chat_history(
  chat_history,
  message,
  role = c("auto", "user", "assistant", "system", "tool"),
  tool_result = NULL
)
}
\arguments{
\item{chat_history}{A single string, a data.frame which is a valid chat history
(see \verb{[chat_history()]}), a list containing a valid chat history under key
'$chat_history', a \link{tidyprompt-class} object, or NULL

A \code{\link[=chat_history]{chat_history()}} object}

\item{message}{A character string representing the message to add}

\item{role}{A character string representing the role of the message sender.
One of: \itemize{
\item "auto": The function automatically determines the role. If the last message
was from the user, the role will be "assistant". If the last message was from anything
else, the role will be "user"
\item "user": The message is from the user
\item "assistant": The message is from the assistant
\item "system": The message is from the system
\item "tool": The message is from a tool (e.g., indicating the result of a function call)
}}

\item{tool_result}{A logical indicating whether the message is a tool result
(e.g., the result of a function call)}
}
\value{
A \code{\link[=chat_history]{chat_history()}} object with the message added as the last row
}
\description{
This function appends a message to a \code{\link[=chat_history]{chat_history()}} object.
The function can automatically determine the role of the message to be
added based on the last message in the chat history. The role can also be
manually specified.
}
\details{
The chat_history object may be of different types:
\itemize{
\item A single string: The function will create a new chat history object
with the string as the first message; the role of that first message will be
"user"
\item A data.frame: The function will append the message to the data.frame.
The data.frame must be a valid chat history; see \code{\link[=chat_history]{chat_history()}}
\item A list: The function will extract the chat history from the list.
The list must contain a valid chat history under the key 'chat_history'.
This may typically be the result from \code{\link[=send_prompt]{send_prompt()}} when using
'return_mode = "full"'
\item A Tidyprompt object (\linkS4class{tidyprompt}): The function will extract the chat history
from the object. This will be done by concatenating the 'system_prompt',
'chat_history', and 'base_prompt' into a chat history data.frame. Note
that the other properties of the \linkS4class{tidyprompt} object will be lost
\item NULL: The function will create a new chat history object
with no messages; the message will be the first message
}
}
\examples{
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
}
