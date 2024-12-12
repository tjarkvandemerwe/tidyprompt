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
#' @keywords internal
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
#' @keywords internal
chat_history.character <- function(chat_history) {
  if (length(chat_history) != 1) {
    stop("A single character string is expected for chat history input.")
  }
  chat_data <- data.frame(
    role = "user",
    content = chat_history,
    tool_result = FALSE,
    stringsAsFactors = FALSE
  )
  class(chat_data) <- c("chat_history", class(chat_data))
  return(chat_data)
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
#' @keywords internal
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

  # Add tool_result column if missing
  if (!"tool_result" %in% names(chat_history)) {
    chat_history$tool_result <- FALSE
  }

  class(chat_history) <- c("chat_history", class(chat_history))
  return(chat_history)
}



#' Add a message to a chat history
#'
#' This function appends a message to a [chat_history()] object.
#' The function can automatically determine the role of the message to be
#' added based on the last message in the chat history. The role can also be
#' manually specified.
#'
#' @details The chat_history object may be of different types:
#' \itemize{
#' \item A single string: The function will create a new chat history object
#' with the string as the first message; the role of that first message will be
#' "user"
#' \item A data.frame: The function will append the message to the data.frame.
#' The data.frame must be a valid chat history; see [chat_history()]
#' \item A list: The function will extract the chat history from the list.
#' The list must contain a valid chat history under the key 'chat_history'.
#' This may typically be the result from [send_prompt()] when using
#' 'return_mode = "full"'
#' \item A Tidyprompt object ([tidyprompt-class]): The function will extract the chat history
#' from the object. This will be done by concatenating the 'system_prompt',
#' 'chat_history', and 'base_prompt' into a chat history data.frame. Note
#' that the other properties of the [tidyprompt-class] object will be lost
#' \item NULL: The function will create a new chat history object
#' with no messages; the message will be the first message
#' }
#'
#' @param chat_history A single string, a data.frame which is a valid chat history
#' (see `[chat_history()]`), a list containing a valid chat history under key
#' '$chat_history', a \link{tidyprompt-class} object, or NULL
#'
#' A [chat_history()] object
#'
#' @param message A character string representing the message to add
#'
#' @param role A character string representing the role of the message sender.
#' One of: \itemize{
#' \item "auto": The function automatically determines the role. If the last message
#' was from the user, the role will be "assistant". If the last message was from anything
#' else, the role will be "user"
#' \item "user": The message is from the user
#' \item "assistant": The message is from the assistant
#' \item "system": The message is from the system
#' \item "tool": The message is from a tool (e.g., indicating the result of a function call)
#' }
#'
#' @param tool_result A logical indicating whether the message is a tool result
#' (e.g., the result of a function call)
#'
#' @return A [chat_history()] object with the message added as the last row
#' @export
#'
#' @example inst/examples/chat_history.R
add_msg_to_chat_history <- function(
    chat_history,
    message,
    role = c("auto", "user", "assistant", "system", "tool"),
    tool_result = NULL
) {
  if (is.null(chat_history)) {
    chat_history <- data.frame(
      role = character(),
      content = character(),
      stringsAsFactors = FALSE
    )
  }
  else if (inherits(chat_history, "Tidyprompt")) {
    chat_history <- chat_history$get_chat_history()
  } else if (
    is.list(chat_history)
    & !is.data.frame(chat_history)
    & "chat_history" %in% names(chat_history)
  ) {
    chat_history <- chat_history$chat_history |> chat_history()
  } else {
    chat_history <- chat_history(chat_history)
  }

  role <- match.arg(role)

  stopifnot(
    is.character(message), length(message) == 1,
    is.character(role), length(role) == 1,
    is.null(tool_result) || is.logical(tool_result)
  )

  # Automatically determine the role if set to "auto"
  if (role == "auto") {
    if (nrow(chat_history) == 0) {
      role <- "user"
    } else {
      role <- ifelse(
        chat_history[nrow(chat_history), "role"] == "user",
        "assistant",
        "user"
      )
    }
  }

  if (is.null(tool_result)) {
    tool_result <- FALSE
  }

  chat_history <- chat_history |>
    dplyr::bind_rows(data.frame(
      role = role,
      content = message,
      tool_result = tool_result
    ))

  return(chat_history)
}
