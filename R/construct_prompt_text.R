#' Construct the prompt text
#'
#' @param prompt A single character string,
#' or a prompt_list object.
#'
#' @return The constructed prompt text as a character string.
#' The prompt text is constructed by applying the modifying functions
#' of all prompt wrappers in the prompt list to the base prompt text.
#' @export
construct_prompt_text <- function(prompt) {
  prompt_list <- prompt_list(prompt)
  prompt_list$construct_prompt_text()
}
