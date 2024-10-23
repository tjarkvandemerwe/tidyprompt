#' Validate chat history
#'
#' This function validates the chat history, ensuring that it is a dataframe
#' that matches the expected format. If a single message is provided as a
#' character string, it will be turned into a valid chat history dataframe.
#'
#' @param chat_history A dataframe with 'role' and 'content' columns,
#' where 'role' is the role of the message ('user' or 'assistant') and 'content'
#' is the content of the message.
#' @param last_message_from_user A logical indicating whether the last message
#' should be from the user. Default is FALSE.
#'
#' Alternatively, you can provide a single message as a character string.
#'
#' @return If valid, the input object will be returned as is (invisibly).
#'
#' If a single message is provided, it will be turned into a valid chat_history dataframe
#' (also returned invisibly).
#'
#' If not valid, an error will be thrown and nothing will be returned.
#'
#' @export
validate_chat_history <- function(chat_history, last_message_from_user = FALSE) {
  input_should_be <- paste0(
    "The input for chat_history must be a dataframe with 'role' and 'content' columns,",
    " where 'role' is the role of the message (e.g., 'system', 'user', or 'assistant') and 'content'",
    " is the content of the message. Alternatively, you can provide a single message",
    " as a character string."
  )

  if (is.null(chat_history) | length(chat_history) == 0)
    stop(input_should_be)

  if (is.character(chat_history) & length(chat_history) > 1)
    stop(input_should_be)

  if (is.character(chat_history)) # Turn single message into valid chat_history
    return(invisible(data.frame(
      role = "user",
      content = chat_history
    )))

  if (
    !is.data.frame(chat_history) |
    ncol(chat_history) != 2 |
    !("role" %in% colnames(chat_history) && "content" %in% colnames(chat_history))
  )
    stop(input_should_be)

  # Check if input has at least one row
  if (nrow(chat_history) == 0)
    stop("The input dataframe for chat_history must have at least one row.")

  # Check that the last message is from the user
  if (utils::tail(chat_history$role, 1) != "user" & last_message_from_user)
    stop("The last message must be from the role 'user'.")

  return(invisible(chat_history))
}
