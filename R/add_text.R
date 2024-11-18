#' Add text to a tidyprompt
#'
#' Add text to a prompt by adding a [prompt_wrap()] which will append the text to
#' the before or after the current prompt text.
#'
#' @param prompt A single string or a [tidyprompt()] object
#' @param text Text to be added to the current prompt text
#' @param sep Separator to be used between the current prompt text and the text to be added
#' @param position Where to add the text; either "after" or "before".
#
#' @return A [tidyprompt()] with an added [prompt_wrap()] which
#' will append the text to the end of the current prompt text
#'
#' @export
#' @example inst/examples/add_text.R
#'
#' @family other_prompt_wraps
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
