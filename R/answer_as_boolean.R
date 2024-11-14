#' Make LLM answer as a boolean (TRUE or FALSE)
#'
#' @param prompt A single string or a [tidyprompt()] object
#' @param add_instruction_to_prompt (optional) Add instruction for replying
#' as a boolean to the prompt text. Set to FALSE for debugging if extractions/validations
#' are working as expected (without instruction the answer should fail the
#' validation function, initiating a retry)
#'
#' @return A [tidyprompt()] with an added [prompt_wrap()] which
#' will ensure that the LLM response is a boolean
#'
#' @export
#'
#' @family answer_as
answer_as_boolean <- function(
    prompt,
    add_instruction_to_prompt = TRUE
) {
  instruction <- "You must answer with only TRUE or FALSE (use no other characters)."

  # Define modification/extraction/validation functions:
  modify_fn <- function(original_prompt_text) {
    if (!add_instruction_to_prompt) {
      return(original_prompt_text)
    }

    glue::glue("{original_prompt_text}\n\n{instruction}")
  }

  extraction_fn <- function(x) {
    normalized <- tolower(trimws(x))
    if (normalized %in% c("true", "false")) {
      return(as.logical(normalized))
    }
    return(llm_feedback(instruction))
  }

  prompt_wrap(prompt, modify_fn, extraction_fn)
}
