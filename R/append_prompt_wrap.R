#' Append a prompt wrap to a prompt object
#'
#' This function appends a prompt wrap (wrapper) to a prompt object. The method
#' can handle different types of input, such as a character or a prompt.
#'
#' @param prompt The prompt to which the wrap will be appended. This can be
#' either a character string or an existing prompt.
#' @param prompt_wrap The wrap to append. This is typically an additional
#' formatting or structure element to be included in the prompt.
#' @return A list combining the prompt and the appended wrap.
#' @export
append_prompt_wrap <- function(prompt, prompt_wrap) {
  UseMethod("append_prompt_wrap")
}

#' @describeIn append_prompt_wrap Default method for appending a prompt wrap.
#'
#' This method handles general cases where the `prompt` is not of
#' class `prompt`. It attempts to convert the `prompt` to a 'prompt',
#' and then calls the `append_prompt_wrap` method for `prompt` objects.
#'
#' @param prompt A prompt object to which the wrap is added.
#' @param prompt_wrap An additional structure to append to the prompt.
#' @return A list combining the prompt and the appended wrap.
#' @export
#' @exportS3Method append_prompt_wrap default
append_prompt_wrap.default <- function(prompt, prompt_wrap) {
  prompt <- prompt(prompt)
  append_prompt_wrap(prompt, prompt_wrap)
}

#' @describeIn append_prompt_wrap Method for appending a prompt wrap to a prompt.
#'
#' @param prompt A prompt object to which the wrap is added.
#' @param prompt_wrap An additional structure to append to the prompt.
#'
#' @return A list combining the prompt and the appended wrap.
#' @export
#' @exportS3Method append_prompt_wrap prompt
append_prompt_wrap.prompt <- function(prompt, prompt_wrap) {
  if (!inherits(prompt_wrap, "prompt_wrap"))
    stop(paste0(
      "The prompt_wrap must be an object of class 'prompt_wrap'",
      " Create a prompt_wrap object with the prompt_wrap() function"
    ))

  prompt$prompt_wraps[[length(prompt$prompt_wraps) + 1]] <- prompt_wrap

  return(prompt)
}
