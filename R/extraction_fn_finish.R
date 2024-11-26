#' Extraction function to extract final response from 'FINISH[...]'
#'
#' This is a helper function which assists in extracting the final response from
#' a large language model after it has been instructed to give this within
#' 'FINISH[...]' (as is done by [answer_by_chain_of_thought()] and
#' [answer_by_react()]). If something cannot be extracted from within the brackets,
#' it falls back on attempting to extract everything after the final occurence
#' of a 'FINISH' variant.
#'
#' @param llm_response Response of a large language model
#'
#' @return Extracted final answer
#'
#' @noRd
#' @keywords internal
extraction_fn_finish <- function(llm_response) {
  if (!extract_from_finish_brackets) {
    return(llm_response)
  }

  # First attempt: Extract text between FINISH[...]
  extracted_response <- stringr::str_extract(
    llm_response,
    "(?si)(?<=FINISH\\[).+?(?=\\])"
  )

  # If extraction fails, try alternative method
  if (is.na(extracted_response)) {
    # Use regex to match 'FINISH' variants and capture text after any punctuation
    pattern <- "(?si)\\bFINISH\\w*\\W*(.*)"
    matches <- stringr::str_match_all(llm_response, pattern)[[1]]

    if (nrow(matches) > 0) {
      # Get the last match
      extracted_response <- matches[nrow(matches), 2]
      extracted_response <- stringr::str_trim(extracted_response)
    } else {
      # If no 'FINISH' variants are found
      extracted_response <- NA
    }
  }

  # Final check for successful extraction
  if (
    is.na(extracted_response) ||
    tolower(trimws(extracted_response)) %in% c("answer", "final answer")
  ) {
    return(llm_feedback(glue::glue(
      "Error, could not parse your final answer.\n",
      "Please type: 'FINISH[<put here your final answer to the original prompt>]'"
    )))
  }

  return(extracted_response)
}
