#' Make LLM answer as an integer (between min and max)
#'
#' @param prompt A single string or a [tidyprompt()] object
#' @param min (optional) Minimum value for the integer
#' @param max (optional) Maximum value for the integer
#' @param add_instruction_to_prompt (optional) Add instruction for replying
#' as an integer to the prompt text. Set to FALSE for debugging if extractions/validations
#' are working as expected (without instruction the answer should fail the
#' validation function, initiating a retry)
#'
#' @return A [tidyprompt()] with an added [prompt_wrap()] which
#' will ensure that the LLM response is an integer.
#'
#' @export
#'
#' @example inst/examples/answer_as_integer.R
#'
#' @family pre_built_prompt_wraps
#' @family answer_as_prompt_wraps
answer_as_integer <- function(
    prompt,
    min = NULL,
    max = NULL,
    add_instruction_to_prompt = TRUE
) {
  instruction <- "You must answer with only an integer (use no other characters)."

  if (!is.null(min) && !is.null(max)) {
    instruction <- paste(instruction, glue::glue(
      "Enter an integer between {min} and {max}."
    ))
  } else if (!is.null(min)) {
    instruction <- paste(instruction, glue::glue(
      "Enter an integer greater than or equal to {min}."
    ))
  } else if (!is.null(max)) {
    instruction <- paste(instruction, glue::glue(
      "Enter an integer less than or equal to {max}."
    ))
  }


  # Define modification/extraction/validation functions:
  modify_fn <- function(original_prompt_text) {
    if (!add_instruction_to_prompt) {
      return(original_prompt_text)
    }

    glue::glue("{original_prompt_text}\n\n{instruction}")
  }

  extraction_fn <- function(x) {
    extracted <- suppressWarnings(as.numeric(x))
    if (is.na(extracted)) {
      return(llm_feedback(instruction))
    }
    return(extracted)
  }

  validation_fn <- function(x) {
    if (x != floor(x)) { # Not a whole number
      return(llm_feedback(instruction))
    }

    if (!is.null(min) && x < min) {
      return(llm_feedback(glue::glue(
        "The number should be greater than or equal to {min}."
      )))
    }
    if (!is.null(max) && x > max) {
      return(llm_feedback(glue::glue(
        "The number should be less than or equal to {max}."
      )))
    }
    return(TRUE)
  }

  prompt_wrap(prompt, modify_fn, extraction_fn, validation_fn)
}
