#' Add chain-of-thought mode to a prompt
#'
#' @param prompt_wrap_or_list A single string, a prompt wrap,
#' or a list of prompt wraps.
#' @param extract_from_finish_brackets A logical indicating whether the final answer
#' should be extracted from the text inside the "FINISH[...]" brackets.
#'
#' @return A prompt list with the chain-of-thought mode added.
#' @export
set_mode_chainofthought <- function(
    prompt_wrap_or_list,
    extract_from_finish_brackets = TRUE
) {
  prompt_list <- validate_prompt_list(prompt_wrap_or_list)

  new_wrap <- create_prompt_wrap(
    type = "mode",

    modify_fn = function(original_prompt_text, modify_fn_args) {
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

      if (modify_fn_args$extract_from_finish_brackets) {
        new_prompt <- glue::glue(
          "{new_prompt}

          When you are done, you must type:
          FINISH[<put here your final answer to the user's prompt>]

          Make sure your final answer follows the logical conclusion of your thought process."
        )
      }

      return(new_prompt)
    },
    modify_fn_args = list(extract_from_finish_brackets = extract_from_finish_brackets),

    extractor_functions = list(function(llm_response) {
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
    })
  )

  return(c(prompt_list, list(new_wrap)))
}

# Code to test extractors/validators
if (FALSE) {

  "Hi!" |>
    add_text("Can you please calculate what is 5+5? Write the answer out as a word") |>
    answer_as_integer(add_instruction_to_prompt = FALSE) |>
    set_llm_provider(create_ollama_llm_provider()) |>
    set_mode_chainofthought(extract_from_finish_brackets = FALSE) |>
    send_prompt()

  "Hi!" |>
    add_text("Can you please calculate what is 5+5? Write the answer out as a word") |>
    answer_as_integer(add_instruction_to_prompt = FALSE) |>
    set_llm_provider(create_ollama_llm_provider()) |>
    set_mode_chainofthought(extract_from_finish_brackets = FALSE) |>
    send_prompt()

  "Hi!" |>
    send_prompt(
      llm_provider = create_ollama_llm_provider(),
      system_prompt = "You are an assistant who always answers in poems. You are also very angry."
    )

  "Hi" |>
    send_prompt(llm_provider = create_ollama_llm_provider())

}
