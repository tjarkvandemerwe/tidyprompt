#' Set chain of thought mode for a prompt
#'
#' @param prompt A prompt object or a single string
#' @param extract_from_finish_brackets A logical indicating whether the final answer
#' should be extracted from the text inside the "FINISH[...]" brackets.
#'
#' @return A prompt with the chain-of-thought mode added.
#' @export
answer_by_chain_of_thought <- function(
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

  prompt_wrap(prompt, modify_fn, extraction_fn, type = "mode")
}
