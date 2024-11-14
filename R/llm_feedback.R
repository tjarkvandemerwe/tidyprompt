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
#' @param tool_result A logical indicating whether the feedback is a tool result.
#' If TRUE, it will be handled differently by [send_prompt()], by presenting the
#' text as a 'system' message. This ensures it will not be
#' filtered out when cleaning the context window in [send_prompt()].
#'
#' @return An object of class "llm_feedback" containing the feedback text.
#'
#' @export
#'
#' @family prompt_wrap_creation
llm_feedback <- function(text, tool_result = FALSE) {
  if (!tool_result) {
    return(structure(
      text,
      class = "llm_feedback"
    ))
  } else {
    return(structure(
      list(
        text = text
      ),
      class = "llm_feedback_tool_result"
    ))
  }
}
