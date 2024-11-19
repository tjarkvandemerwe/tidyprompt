#' Set system prompt
#'
#' Set the system prompt for a prompt. The system prompt will be added
#' as a message with role 'system' at the start of the chat history when
#' this prompt is evaluated by [send_prompt()].
#'
#' @details The system prompt will be stored in the [tidyprompt()] object
#' as '$system_prompt'.
#'
#' @param prompt A single string or a [tidyprompt()] object
#' @param system_prompt A single character string representing the system prompt
#'
#' @return A [tidyprompt()] with the system prompt set
#'
#' @export
#'
#' @example inst/examples/set_system_prompt.R
#'
#' @family prompt_wrap
#' @family pre_built_prompt_wraps
#' @family miscellaneous_prompt_wraps
set_system_prompt <- function(prompt, system_prompt) {
  prompt <- tidyprompt(prompt)

  if (!is.character(system_prompt) |
      length(system_prompt) != 1
  )
    stop("system_prompt must be a single character string")

  prompt$system_prompt <- system_prompt

  return(prompt)
}
