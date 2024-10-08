#' Create a new prompt object
#'
#' This function constructs a `prompt` object that contains the question,
#' expected reply format, a function to extract the reply, and validation
#' functions to ensure the response is valid.
#'
#' @param prompt A string containing the question or prompt text.
#' @param reply_format A string describing the desired format or structure of the reply (optional).
#' @param reply_extract A function that extracts the reply from the full response (optional).
#' @param validation A list of validation functions, each checking the extracted response for validity.
#'   If any function returns `FALSE`, the prompt is sent again, including the previous response
#'   and validation error (optional).
#'
#' @return A `prompt` object with the specified components.
#'
#' @examples
#' # Create a simple prompt with no validation
#' p <- prompt("What is your name?")
#'
#' # Create a prompt with a format, reply extraction, and validation
#' p <- prompt(
#'   "Enter your age:",
#'   reply_format = "numeric",
#'   reply_extract = as.numeric,
#'   validation = list(function(x) x > 0)
#' )
#'
#' @export
new_prompt <- function(prompt,
                       reply_format = NULL,
                       reply_extract = NULL,
                       validation = NULL) {

  # Input validation
  if (!is.character(prompt) || length(prompt) != 1) {
    stop("`prompt` must be a single string.")
  }

  if (!is.null(reply_format) && !is.character(reply_format)) {
    stop("`reply_format` must be a string or NULL.")
  }

  if (!is.null(reply_extract) && !is.function(reply_extract)) {
    stop("`reply_extract` must be a function or NULL.")
  }

  if (!is.null(validation)) {
    if (!is.list(validation) || !all(vapply(validation, is.function, logical(1)))) {
      stop("`validation` must be a list of functions or NULL.")
    }
  }

  # Construct and return the prompt object
  structure(
    list(
      prompt = prompt,
      reply_format = reply_format,
      reply_extract = reply_extract,
      validation = validation
    ),
    class = "prompt"
  )
}

#' User-friendly prompt constructor
#'
#' A wrapper for `new_prompt()` that allows for creating prompts more easily.
#'
#' @inheritParams new_prompt
#' @return A `prompt` object.
#' @export
prompt <- function(prompt,
                   reply_format = NULL,
                   reply_extract = NULL,
                   validation = NULL) {
  new_prompt(prompt, reply_format, reply_extract, validation)
}
