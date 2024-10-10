#' ...
#'
#' @param prompt ...
#' @param llm_provider ...
#' @param extractors ...
#' @param validations ...
#' @param max_retries ...
#' @param verbose ....
#'
#' @return ...
#' @export
query_llm <- function(
    prompt,
    llm_provider = NULL,
    extractors = list(),
    validations = list(),
    max_retries = 10,
    verbose = TRUE
) {
  prompt <- validate_prompt_list(prompt)

  last_llm_provider_from_prompt_list <- get_llm_provider_from_prompt_list(prompt)
  if (!is.null(last_llm_provider_from_prompt_list))
    llm_provider <- last_llm_provider_from_prompt_list
  if (is.null(llm_provider))
    stop("No llm_provider provided and no llm_provider found in prompt list.")

  # Create internal chat_history
  chat_history <- data.frame(
    role = character(),
    content = character()
  )
  # Create internal function to send_chat to LLM-provider
  send_chat <- function(message) {
    # TODO: logging, verbose printing of queries & responses
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

  # Retrieve initial response
  response <- send_chat(prompt |> construct_prompt_text())

  # TODO: apply extractors, validators --- altering the response as needed
  # (get from prompt object if provided; likely want to reorder based on type)
  # ...

  # OLD CODE (from project):
  if (FALSE) {
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
  }


  return(response)
}
