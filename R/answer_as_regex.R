#' Make LLM answer match a specific regex
#'
#' @param prompt A single string or a [tidyprompt()] object
#' @param regex A character string specifying the regular expression the response must match
#'
#' @return A [tidyprompt()] with an added [prompt_wrap()] which will ensure
#' that the LLM response matches the specified regex
#'
#' @export
#'
#' @example inst/examples/answer_as_regex.R
#'
#' @family answer_as
answer_as_regex <- function(
    prompt,
    regex
) {
  # Default instruction for LLM if regex is provided
  instruction <- glue::glue(
    "You must answer with a response that matches this format: {regex}."
  )

  modify_fn <- function(original_prompt_text) {
    glue::glue("{original_prompt_text}\n\n{instruction}")
  }

  extraction_fn <- function(x) {
    if (grepl(regex, x)) {
      return(x)
    }
    return(llm_feedback(instruction))
  }

  prompt_wrap(prompt, modify_fn, extraction_fn)
}
