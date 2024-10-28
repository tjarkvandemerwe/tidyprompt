#' Create or validate chat history object
#'
#' This function creates and validates a chat history object, ensuring it matches
#' the expected format with 'role' and 'content' columns. It has separate methods
#' for data.frame and character inputs and includes a helper function to add a
#' system prompt to the chat history.
#'
#' @param chat_history A single string, a dataframe with 'role' and 'content' columns,
#' or NULL.
#'
#' @return A valid chat history dataframe (of class 'chat_history').
#'
#' @export
chat_history <- function(chat_history) {
  UseMethod("chat_history")
}

#' @exportS3Method chat_history default
chat_history.default <- function(chat_history) {
  stop("The input must be either a data frame with 'role' and 'content' columns, or a single character string.")
}

#' @exportS3Method chat_history character
chat_history.character <- function(chat_history) {
  if (length(chat_history) != 1) {
    stop("A single character string is expected for chat history input.")
  }
  chat_data <- data.frame(
    role = "user",
    content = chat_history,
    stringsAsFactors = FALSE
  )
  class(chat_data) <- c("chat_history", class(chat_data))
  return(invisible(chat_data))
}

#' @exportS3Method chat_history data.frame
chat_history.data.frame <- function(chat_history) {
  if (!all(c("role", "content") %in% names(chat_history)) || ncol(chat_history) != 2) {
    stop("The data frame must contain exactly two columns: 'role' and 'content'.")
  }
  if (!all(chat_history$role %in% c("user", "assistant", "system"))) {
    stop("The 'role' column must contain only 'user', 'assistant', or 'system'.")
  }
  if (!is.character(chat_history$content)) {
    stop("The 'content' column must be of type character.")
  }
  class(chat_history) <- c("chat_history", class(chat_history))
  return(invisible(chat_history))
}
