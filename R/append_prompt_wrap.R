#' Append a prompt wrap to a prompt object
#'
#' This function appends a prompt wrap (wrapper) to a prompt object. The method
#' can handle different types of input, such as a character or a prompt list.
#'
#' @param prompt The prompt to which the wrap will be appended. This can be
#' either a character string or an existing prompt list.
#' @param prompt_wrap The wrap to append. This is typically an additional
#' formatting or structure element to be included in the prompt.
#' @return A list combining the prompt and the appended wrap.
#' @export
append_prompt_wrap <- function(prompt, prompt_wrap) {
  UseMethod("append_prompt_wrap")
}

#' @describeIn append_prompt_wrap Default method for appending a prompt wrap.
#'
#' This method handles general cases where the `prompt` is not of a specific class
#' (e.g., character). It combines the prompt and wrap into a list.
#'
#' @param prompt A prompt object to which the wrap is added.
#' @param prompt_wrap An additional structure to append to the prompt.
#' @return A list combining the prompt and the appended wrap.
#' @export
#' @exportS3Method append_prompt_wrap.default
append_prompt_wrap.default <- function(prompt, prompt_wrap) {

  prompt_wraps <- c(prompt, list(prompt_wrap))

  # Retain class
  class(prompt_wraps) <- class(prompt)

  return(prompt_wraps)

}

#' @describeIn append_prompt_wrap Method for character prompts.
#'
#' This method converts a character string prompt to a prompt list before appending
#' the wrap.
#'
#' @param prompt A character string representing the initial prompt.
#' @param prompt_wrap A structure or wrapper to append to the character prompt.
#' @return A list combining the prompt (converted from character) and the appended wrap.
#' @export
#' @exportS3Method append_prompt_wrap.character
append_prompt_wrap.character <- function(prompt, prompt_wrap) {
  # character to prompt_list
  print(prompt)
  prompt_list <- prompt_list(prompt)

  append_prompt_wrap(prompt_list, prompt_wrap)
}
