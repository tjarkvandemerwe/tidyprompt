#' Create an `llm_feedback` object
#'
#' This object is used to send feedback to a LLM when a LLM reply
#' does not succesfully pass an extraction or validation function
#' (as handled by [send_prompt()] and defined using [prompt_wrap()]).
#' The feedback text is sent back to the LLM. The extraction or validation function
#' should then return this object with the feedback text that should be sent to the LLM.
#'
#' @param text A character string containing the feedback text. This will
#' be sent back to the LLM after not passing an extractor or validator function
#'
#' @param tool_result A logical indicating whether the feedback is a tool result.
#' If TRUE, [send_prompt()] will not remove it from the chat history when
#' cleaning the context window during repeated interactions
#'
#' @return An object of class "llm_feedback" (or "llm_feedback_tool_result")
#' containing the feedback text to send back to the LLM
#'
#' @export
#'
#' @example inst/examples/llm_feedback.R
#'
#' @family prompt_wrap
#' @family prompt_evaluation
#' @seealso [llm_break()]
llm_feedback <- function(text, tool_result = FALSE) {
  if (!tool_result) {
    return(
      structure(
        text,
        class = "llm_feedback"
      )
    )
  } else {
    return(
      structure(
        list(
          text = text
        ),
        class = "llm_feedback_tool_result"
      )
    )
  }
}
