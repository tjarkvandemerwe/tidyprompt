#' Send a prompt to a LLM provider
#'
#' @param prompt A prompt object or a single string
#' @param llm_provider 'llm_provider' object
#' @param verbose If the interaction with the LLM provider should be printed
#' @param extract_validate_mode ...
#'
#' @return ...
#' @export
send_prompt <- function(
    prompt,
    llm_provider,
    verbose = getOption("tidyprompt.verbose", FALSE),
    extract_validate_mode = c("extraction_then_validation", "wrap_by_wrap")
) {
  ## 1 Validate arguments

  prompt <- prompt(prompt)
  extract_validate_mode <- match.arg(extract_validate_mode)


  ## 2 Retrieve prompt evaluation settings

  extractions_validations <- get_extractions_and_validations(prompt)
  extraction_functions <- extractions_validations$extractions
  validation_functions <- extractions_validations$validations


  ## 3 Chat_history & send_chat

  create_chat_df <- function(role = character(), content = character()) {
    data.frame(role = role, content = content)
  }

  # Create internal chat_history for this prompt
  chat_history <- create_chat_df()

  # Add system prompt
  if (!is.null(prompt$system_prompt))
    chat_history <- create_chat_df("system", prompt$system_prompt)

  # Create internal function to send_chat to the given LLM-provider
  send_chat <- function(message) {
    message <- as.character(message)

    if (verbose) {
      message("--- Sending message to LLM-provider: ---")
      message(message)
    }

    chat_history <<- chat_history |>
      dplyr::bind_rows(create_chat_df("user", message)) # TODO: remove dplyr

    completion <- llm_provider$complete_chat(chat_history)

    if (verbose) {
      message("--- Received response from LLM-provider: ---")
      message(completion$content)
    }

    chat_history <<- chat_history |>
      dplyr::bind_rows(create_chat_df(completion$role, completion$content))

    return(invisible(completion$content))
  }


  ## 4 Retrieve initial response

  response <- prompt |> construct_prompt_text() |> send_chat()


  ## 5 Apply extractions and validations

  # TODO: wrap by wrap mode
  max_retries <- 10 # TODO: use max_retries from wraps in prompt object

  # Mode for first all extractions, then all validations:
  if (extract_validate_mode == "extraction_then_validation") {
    tries <- 0; successful_output <- FALSE
    while (tries < max_retries & !successful_output) {
      tries <- tries + 1

      # Apply extraction functions
      extraction_error <- FALSE
      if (length(extraction_functions) > 0) {
        for (i in 1:length(extraction_functions)) {
          extraction_function <- extraction_functions[[i]]

          # If extraction function has attribute 'tool_functions':
          if (!is.null(attr(extraction_function, "tool_functions"))) {
            tool_functions <- attr(extraction_function, "tool_functions")
            extraction_result <- extraction_function(response, tool_functions)
          } else {
            extraction_result <- extraction_function(response)
          }

          # If it inherits llm_feedback, send the feedback to the LLM & get new response
          if (inherits(extraction_result, "llm_feedback")) {
            extraction_error <- TRUE
            response <- send_chat(extraction_result)
            break
          }

          # If no llm_feedback, extraction was succesful
          response <- extraction_result
        }
      }
      if (extraction_error) next

      # Apply validation functions
      validation_error <- FALSE
      if (length(validation_functions) > 0) {
        for (i in 1:length(validation_functions)) {
          validation_function <- validation_functions[[i]]
          validation_result <- validation_function(response)

          # If it inherits llm_feedback, send the feedback to the LLM & get new response
          if (inherits(validation_result, "llm_feedback")) {
            validation_error <- TRUE
            response <- send_chat(validation_result)
            break
          }
        }
      }
      if (validation_error) next

      # If no errors, break the loop
      successful_output <- TRUE
    }
  }

  # Wrap by wrap
  if (extract_validate_mode == "wrap_by_wrap") {
    # TODO: implementation
  }


  ## 7 Final evaluation

  if (!successful_output)
    stop("Failed to reach a valid answer after ", max_retries, " tries.")

  return(response)
}
