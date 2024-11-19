#' Create or validate `chat_history` object
#'
#' This function creates and validates a `chat_history` object, ensuring that it matches
#' the expected format with 'role' and 'content' columns. It has separate methods
#' for `data.frame` and `character` inputs and includes a helper function to add a
#' system prompt to the chat history.
#'
#' @param chat_history A single string, a `data.frame` with 'role' and 'content' columns,
#' or NULL. If a `data.frame` is provided, it should contain 'role' and 'content' columns,
#' where 'role' is either 'user', 'assistant', or 'system', and 'content' is a character string
#' representing a chat message
#'
#' @return A valid chat history `data.frame` (of class `chat_history`)
#'
#' @export
#'
#' @example inst/examples/chat_history.R
#'
#' @family chat_history
chat_history <- function(chat_history) {
  UseMethod("chat_history")
}



#' Default method for `chat_history()`
#'
#' Calls error which indicates that the input was not a `character` or `data.frame`.
#'
#' @details When input is a `character` or `data.frame`, the appropriate method will be called
#' (see `[chat_history.character()] and [chat_history.data.frame()]).
#'
#' @param chat_history Object which is not `character` or `data.frame`
#'
#' @exportS3Method chat_history default
chat_history.default <- function(chat_history) {
  stop("The input must be either a data frame with 'role' and 'content' columns, or a single string.")
}



#' Method for `chat_history()` when the input is a single string
#'
#' Creates a `chat_history` object from a single string.
#'
#' @param chat_history A single string
#'
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



#' Method for `chat_history()` when the input is a `data.frame`
#'
#' Creates a `chat_history` object from a data frame.
#'
#' @param chat_history A data frame with 'role' and 'content' columns,
#' where 'role' is either 'user', 'assistant', or 'system', and 'content' is a character string
#' representing a chat message
#'
#' @exportS3Method chat_history data.frame
chat_history.data.frame <- function(chat_history) {
  if (!all(c("role", "content") %in% names(chat_history))) {
    stop("The data frame must contain'role' and 'content' columns.")
  }
  if (!all(chat_history$role %in% c("user", "assistant", "system"))) {
    stop("The 'role' column must contain only 'user', 'assistant', or 'system'.")
  }
  if (!is.character(chat_history$role)) {
    stop("The 'role' column must be of type character.")
  }
  if (!is.character(chat_history$content)) {
    stop("The 'content' column must be of type character.")
  }
  class(chat_history) <- c("chat_history", class(chat_history))
  return(invisible(chat_history))
}
