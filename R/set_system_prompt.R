#' Set system prompt
#'
#' Set the system prompt for a prompt
#'
#' @param prompt A prompt object
#' @param system_prompt A single character string representing the system prompt
#'
#' @return A prompt object with the system prompt set
#' @export
set_system_prompt <- function(prompt, system_prompt) {
  prompt <- prompt(prompt)

  if (!is.character(system_prompt) |
      length(system_prompt) != 1
  )
    stop("system_prompt must be a single character string")

  prompt$system_prompt <- system_prompt

  return(prompt)
}
