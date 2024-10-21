#' Add chain-of-thought mode to a prompt
#'
#' @param prompt A single string, a prompt wrap,
#' or a list of prompt wraps.
#' @param extract_from_finish_brackets A logical indicating whether the final answer
#' should be extracted from the text inside the "FINISH[...]" brackets.
#'
#' @return A prompt with the chain-of-thought mode added.
#' @export
set_mode_chainofthought <- function(
    prompt,
    extract_from_finish_brackets = TRUE
) {

  # Define modification/extraction/validation functions:
  modify_fn <- function(original_prompt_text) {
    new_prompt <- glue::glue(
      "You are given a user's prompt.
        To answer the user's prompt, you need to think step by step to arrive at a final answer.

        ----- START OF USER'S PROMPT -----
        {original_prompt_text}
        ----- END OF USER'S PROMPT -----

        What are the steps you would take to answer the user's prompt?
        Describe your thought process in the following format:
          >> step 1: <step 1 description>
          >> step 2: <step 2 description>
          (etc.)"
    )

    if (extract_from_finish_brackets) {
      new_prompt <- glue::glue(
        "{new_prompt}

          When you are done, you must type:
          FINISH[<put here your final answer to the user's prompt>]

          Make sure your final answer follows the logical conclusion of your thought process."
      )
    }

    return(new_prompt)
  }

  extraction_fn <- function(llm_response) {
    if (!extract_from_finish_brackets) {
      return(llm_response)
    }
    extracted_response <- stringr::str_extract(llm_response, "(?si)(?<=FINISH\\[).+?(?=\\])")

    if (
      is.na(extracted_response) ||
      tolower(extracted_response) == "answer" ||
      tolower(extracted_response) == "final answer"
    ) {
      return(create_llm_feedback(glue::glue(
        "Error, could not parse your final answer.
          Please type: 'FINISH[<put here your final answer to the original prompt>]'"
      )))
    }

    return(extracted_response)
  }

  validation_fn <- function(x) {
    if (!is.null(min) && x < min) {
      return(create_llm_feedback(glue::glue(
        "The number should be greater than or equal to {min}."
      )))
    }
    if (!is.null(max) && x > max) {
      return(create_llm_feedback(glue::glue(
        "The number should be less than or equal to {max}."
      )))
    }
    return(TRUE)
  }

  # Create new wrap:
  new_wrap <- prompt_wrap(
    type = "mode",
    modify_fn = modify_fn,
    extraction_functions = list(extraction_fn)
  )

  # Append wrap to prompt
  prompt <- append_prompt_wrap(prompt, new_wrap)

  return(prompt)
}
