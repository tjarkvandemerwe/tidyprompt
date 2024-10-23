#' Create an llm feedback object
#'
#' This object is used to send feedback to the LLM when a LLM reply
#' does not succesfully pass an extractor or validator function.
#' The extractor or validator function should then return this object
#' with the feedback text that should be sent to the LLM.
#'
#' @param text A character string containing the feedback text. This will
#' be sent back to the LLM after not passing an extractor or validator function.
#'
#' @return An object of class "llm_feedback" containing the feedback text.
#' @export
create_llm_feedback <- function(text) {
  return(structure(
    text,
    class = "llm_feedback"
  ))
}
