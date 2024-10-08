# Idea for design:
# - send_query can take a 'raw' prompt (e.g., just text/chat_history); then
#   the options have to be set in the arguments of this function
# - send_query can take a prompt object, which contains the prompt and the options
#     This prompt object can be build via tidy-style piping functions

# Prompt + evaluation consist of several building blocks, which are functions:
# - prompt modifiers; these take the initial prompt and modify it.
#   Special case is mode modifier (there can only be 1, applied after others)
#   Another special case is tool modifier (there can only be 1, applied also after mode modifier)
# - tool-callers; these take LLM-response, parse for function calls, and execute function + give
#   feedback to LLM. When a tool is called, extracting/validating will not be performed
# - extractors; these take an LLM-response and apply some form of parsing to it,
#   passing forward the extracted information. Can return error to LLM if extraction fails. What is extracted
#   will be the input for validation functions, and, pending validation, will be the
#   final output
# - validators; these take the (extracted) LLM response and validate it via functions
#   that return a boolean. If the validation fails, the error message is passed back to the LLM

# Adding, e.g., chain-of-thought reasoning, can be done by adding a prompt modifier
# that modifies the prompt to include the chain-of-thought reasoning instructions,
# and an extractor that extracts the final answer from the chain-of-thought response (e.g., within 'Finish[]')

# Adding a tool can be done by specifying the tools in the tool modifier,
# and then the tool-caller should parse the LLM-response for tool-calls and execute them

# TODO: integrate the above design, create prompt object
# TODO: integrate llm_provider for completing chat
# TODO: add functionality for tool-calling, extractor functions

if (FALSE) {
  # Old documentation from original send_query:

  #' Send query to LLM with validation and retry mechanism (old documentation from )
  #'
  #' This function sends a query to a large language model (LLM), optionally using chain-of-thought reasoning
  #' and applying validation functions to the responses. It handles retries and error messages in case
  #' of invalid responses.
  #'
  #' @param query A string representing the query to be sent to the LLM.
  #'
  #' @param llm_model A string specifying the LLM model to use. Defaults to `global$settings$llm_model`
  #'
  #' @param mode A character string specifying the mode for the query. Can be
  #'  either `"default"` or `"chain-of-thought"`. In `"chain-of-thought"` mode,
  #'  the LLM is prompted to think step-by-step. Defaults to `"default"`.
  #'
  #' @param validations A list of validation functions to apply to the LLM response.
  #'  Each function should return a logical value (`TRUE` for a valid response and
  #'  `FALSE` for an invalid one). The error messages which will be passed back
  #'  to the LLM need to be either provided in the names() of the list, or as
  #'  attribute 'error_message' to the 'FALSE' value which a validation function
  #'  returns. See examples.
  #'
  #' @param max_retries An integer specifying the maximum number of retry attempts
  #'  in case of an invalid response. Defaults to 10.
  #'
  #' @param verbose A logical flag indicating whether the interaction with the LLM
  #'  should be printed to the console.Defaults to `TRUE`.
  #'
  #' @return The final response from the LLM after validation and retries.
  #'  If all retries fail, an error is raised.
  #'
  #' @throws An error if the LLM response fails to pass validation after `max_retries`
  #'  attempts, or if there are issues with the validation functions.
  #'
  #' @examples
  #'   # Basic query with default settings
  #'   send_query("What is the capital of France?")
  #'
  #'   # Chain-of-thought mode example(
  #'   send_query("What is the square root of 144?", mode = "chain-of-thought")
  #'
  #'   # Example 1 with validation:
  #'   #   error message via names() of 'validations'
  #'   my_validation <- function(response) {
  #'     if (response == "Paris") return(TRUE)
  #'     return(FALSE)
  #'   }
  #'   send_query(
  #'     "What is the capital of France?",
  #'     validations = list(
  #'       "Tell me only the name of the capital, without dots." = my_validation
  #'     ),
  #'     verbose = TRUE
  #'   )
  #'
  #'   # Example 2 with validation:
  #'   #   error message as attribute of 'FALSE' result
  #'   my_validation <- function(response) {
  #'     # Check if the response is only "Paris" (regardless of case)
  #'     if (toupper(response) != "PARIS") {
  #'       return(structure(
  #'         FALSE,
  #'         error_message = "Tell me only the name of the capital, without dots."
  #'       ))
  #'     }
  #'     # Check if the response is uppercase
  #'     if (response != "PARIS") {
  #'       return(structure(
  #'         FALSE,
  #'         error_message = "Tell me in uppercase."
  #'       ))
  #'     }
  #'     return(TRUE)
  #'   }
  #'   send_query(
  #'     "What is the capital of France?",
  #'     validations = list(my_validation),
  #'     verbose = TRUE
  #'   )
  #'
  #'   # See also bottom of 'r/src/functions/send_query.R' for more examples.

}

#' ...
#'
#' @param prompt ...
#' @param llm_model ...
#' @param mode ...
#' @param validations ...
#' @param max_retries ...
#' @param verbose ...
#' @param cot_require_finish_brackets ...
#' @param also_return_chat_history ...
#'
#' @return ...
#' @export
#'
#' @examples
#' # ...
query_llm <- function(
    prompt,
    llm_provider,
    mode = c("default", "chain-of-thought"),
    validations = list(),
    max_retries = 10,
    verbose = global$settings$verbose_llm,
    cot_require_finish_brackets = TRUE,
    also_return_chat_history = FALSE
) {
  mode <- match.arg(mode)

  raw_prompt <- prompt

  # Create internal chat_history
  chat_history <- data.frame(
    role = character(),
    content = character()
  )

  # Create internal function to send_chat to LLM-provider
  send_chat <- function(message) {
    chat_history <<- chat_history |>
      dplyr::bind_rows(data.frame(
        role = "user",
        content = message
      ))

    completion <- llm_provider$complete_chat(chat_history)
    chat_history <<- chat_history |>
      dplyr::bind_rows(data.frame(
        role = completion$role,
        content = completion$content
      ))

    return(invisible(completion$content))
  }


  if (mode == "chain-of-thought") {
    raw_prompt <- glue::glue(
      "{raw_prompt}

      -------------------------------------------------------------------------

      You are given the above prompt.
      To answer this prompt, please think step-by-step, detailing your thought process.
      What are the steps you would take to answer this question?

      Describe your thought process in the following format:
        >> step 1: <description of step 1>
        >> step 2: <description of step 2>
        (etc.)"
    )

    if (cot_require_finish_brackets) {
      raw_prompt <- glue::glue(
        "{raw_prompt}

        When you are done, you must type:
        'Finish[<put here your final answer to the original prompt>]'

        Make sure the final answer follows the logical conclusion of
        the step by step reasoning."
      )
    }
  }

  # Retrieve initial response
  response <- send_chat(raw_prompt)

  # If no parsing/validation required,
  if (mode == "default" & length(validations) == 0)
    return(response)

  # Else start a parsing/validation loop
  tries <- 0; successful_output <- FALSE
  while (tries < max_retries) {
    tries <- tries + 1

    # Attempt to parse final answer from the chain-of-thought response
    #   If it fails, tell LLM how it should provide the answer
    if (mode == "chain-of-thought" & cot_require_finish_brackets) {
      response <- stringr::str_extract(response, "(?s)(?<=Finish\\[).+?(?=\\])")

      # LLM may not have put answer within 'Finish[...]' brackets;
      #   sometimes the LLM also puts 'answer' within the brackets, while the
      #   actual answer is placed somewhere else
      if (is.na(response) | tolower(response) == "answer") {
        response <- send_chat(glue::glue(
          "Error, could not parse final answer.
          Please call 'Finish[<put here your final answer to the original prompt>]'"
        ))
        next
      }
    }

    if (length(validations) == 0) {
      message("No validations, returning response.")
      successful_output <- TRUE
      break
    }

    # Apply validation functions to the response
    #   If they fail, give LLM the error message so it can retry
    all_validations_passed <- TRUE
    for (i in 1:length(validations)) {
      validation_function <- validations[[i]]
      validation_result <- validation_function(response)

      if (!is.logical(validation_result))
        stop("Validation function did not return a logical value.")

      if (!validation_result) {
        all_validations_passed <- FALSE

        if (!is.null(attr(validation_result, "error_message"))) {
          error_message <- attr(validation_result, "error_message")
        } else {
          error_message <- names(validations)[i]
        }

        if (is.null(error_message))
          stop("
              No error message was provided with the validation function.
              Please provide an error message as an attribute to the 'FALSE'
              result of the validation function, or as a named element in the
              'validations' list.
            ")

        response <- send_chat(
          paste0("Error: ", error_message)
        )

        break
      }
    }

    if (all_validations_passed) {
      successful_output <- TRUE
      break
    }
  }

  if (!successful_output)
    stop("Failed to reach a valid answer after ", max_retries, " tries.")

  if (also_return_chat_history)
    return(list(
      response = response,
      chat_history = chat_history
    ))
  return(response)
}
