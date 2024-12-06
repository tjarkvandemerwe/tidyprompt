#' Make LLM answer as a boolean (TRUE or FALSE)
#'
#' @param prompt A single string or a [tidyprompt()] object
#' @param true_definition (optional) Definition of what would constitute TRUE.
#' This will be included in the instruction to the LLM. Should be a single string
#' @param false_definition (optional) Definition of what would constitute FALSE.
#' This will be included in the instruction to the LLM. Should be a single string
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
#' @example inst/examples/answer_as_boolean.R
#'
#' @family pre_built_prompt_wraps
#' @family answer_as_prompt_wraps
answer_as_boolean <- function(
    prompt,
    true_definition = NULL,
    false_definition = NULL,
    add_instruction_to_prompt = TRUE
) {
  instruction <- "You must answer with only TRUE or FALSE (use no other characters)."
  if (!is.null(true_definition))
    instruction <- paste(instruction, glue::glue("TRUE means: {true_definition}."))
  if (!is.null(false_definition))
    instruction <- paste(instruction, glue::glue("FALSE means: {false_definition}."))

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

  prompt_wrap(
    prompt, modify_fn, extraction_fn,
    name = "answer_as_boolean"
  )
}
