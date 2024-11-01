#' Add text to a tidyprompt
#'
#' Add text to a prompt by adding a prompt wrapper to the tidyprompt.
#' The text will be added to the end of the prompt text.
#'
#' @param prompt A single string or a tidyprompt object.
#' @param text Text to be added to the prompt.
#' @param sep Separator to be used between the original prompt text and the added text.
#' @param position Where to add the text; either "after" or "before".
#
#' @return A tidyprompt with an added prompt wrapper object which
#' will append the text to the end of the prompt text.
#' @export
add_text <- function(
    prompt,
    text, position = c("after", "before"), sep = "\n\n"
) {
  position <- match.arg(position)

  modify_fn <- function(original_prompt_text) {
    if (position == "after") {
      paste(original_prompt_text, text, sep = sep)
    } else {
      paste(text, original_prompt_text, sep = sep)
    }
  }

  prompt_wrap(prompt, modify_fn)
}
