#' Parse LLM response as a dataframe with optional dimensions validation
#'
#' @param prompt A single string or a [tidyprompt()] object
#' @param nrow Optional. The expected number of rows in the dataframe. If NULL, no row validation is performed.
#' @param ncol Optional. The expected number of columns in the dataframe. If NULL, no column validation is performed.
#'
#' @return A [tidyprompt()] object that ensures the response is a dataframe with optional dimension checks.
#' @export
answer_as_dataframe <- function(prompt, nrow = NULL, ncol = NULL) {

  modify_fn <- function(prompt_text) {
    dimension_instructions <- ""
    if (!is.null(nrow) && !is.null(ncol)) {
      dimension_instructions <- glue::glue(
        " The table must have exactly {nrow} rows and {ncol} columns."
      )
    } else if (!is.null(nrow)) {
      dimension_instructions <- glue::glue(
        " The table must have exactly {nrow} rows."
      )
    } else if (!is.null(ncol)) {
      dimension_instructions <- glue::glue(
        " The table must have exactly {ncol} columns."
      )
    }

    glue::glue(
      "{prompt_text}\n\n",
      "Your response must be formatted as a JSON object representing a tabular structure, ",
      "with each column as a key and rows as arrays of values. Ensure the response is valid JSON.\n",
      "{dimension_instructions}"
    )
  }

  extraction_fn <- function(llm_response) {
    tryCatch({
      # First extract JSON
      llm_response <- extraction_fn_json(llm_response)

      # Then transform into dataframe
      result <- data.frame(llm_response)
      if (!is.data.frame(result)) {
        stop("Parsed JSON is not a valid dataframe-like structure.")
      }
      result
    }, error = function(e) {
      llm_feedback("Could not extract a valid JSON table. Please try again.")
    })
  }

  validation_fn <- function(extracted_value) {
    if (!is.data.frame(extracted_value)) {
      return(llm_feedback("The extracted value is not a dataframe. Please ensure the JSON table is correctly structured."))
    }

    if (!is.null(nrow) && nrow(extracted_value) != nrow) {
      return(llm_feedback(
        glue::glue("The extracted dataframe has {nrow(extracted_value)} rows, but {nrow} rows were expected.")
      ))
    }

    if (!is.null(ncol) && ncol(extracted_value) != ncol) {
      return(llm_feedback(
        glue::glue("The extracted dataframe has {ncol(extracted_value)} columns, but {ncol} columns were expected.")
      ))
    }

    TRUE
  }

  prompt_wrap(prompt, modify_fn, extraction_fn, validation_fn)
}
