#' Instruct LLM to respond with valid R code
#'
#' @param prompt A single string or a [tidyprompt()] object
#' @param validate_code Logical indicating if basic syntax validation should be applied
#' @param save_location Optional path to save the extracted code as a file
#'
#' @return A [tidyprompt()] object with an added [prompt_wrap()] for R code extraction
#' @export
answer_as_r <- function(prompt, validate_code = TRUE, save_location = NULL) {
  modify_fn <- function(original_prompt_text) {
    glue::glue("{original_prompt_text}\n\nYou must respond with valid R code.")
  }

  extraction_fn <- function(response) {
    # Extract the first code block
    extracted <- stringr::str_extract(response, "(?s)(?<=```r)(.*?)(?=```)")
    if (is.null(extracted)) {
      return(llm_feedback("No valid R code block was found in the response."))
    }
    if (!is.null(save_location)) {
      writeLines(extracted, save_location)
    }
    extracted
  }

  validation_fn <- function(code) {
    if (!validate_code) return(TRUE)
    tryCatch({
      parse(text = code)
      TRUE
    }, error = function(e) {
      llm_feedback("The provided code is not valid R syntax.")
    })
  }

  prompt_wrap(prompt, modify_fn, extraction_fn, validation_fn)
}
