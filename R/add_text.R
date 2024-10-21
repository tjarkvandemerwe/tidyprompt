#' Add text to a prompt
#'
#' Add text to a prompt by appending a prompt wrapper to the prompt list.
#' The text will be added to the end of the prompt text.
#'
#' @param prompt A single string, a prompt_wrap object, or a list
#' of prompt_wrap objects.
#' @param text Text to be added to the prompt.
#' @param sep Separator to be used between the original prompt text and the added text.
#
#' @return A prompt list with an added prompt wrapper object which
#' will append the text to the end of the prompt text.
#' @export
add_text <- function(prompt, text, sep = "\n\n") {
  modify_fn <- function(original_prompt_text) {
    return(paste(original_prompt_text, text, sep = sep))
  }

  new_wrap <- create_prompt_wrap(
    modify_fn = modify_fn
  )

  append_prompt_wrap(prompt, new_wrap)
}
