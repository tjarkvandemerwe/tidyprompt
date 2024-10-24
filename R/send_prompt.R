#' Send a prompt to a LLM provider
#'
#' @param prompt A prompt object or a single string
#' @param llm_provider 'llm_provider' object
#' @param verbose If the interaction with the LLM provider should be printed
#' @param max_interactions Maximum number of interactions before stopping
#'
#' @return ...
#' @export
send_prompt <- function(
    prompt,
    llm_provider,
    max_interactions = 10,
    verbose = getOption("tidyprompt.verbose", FALSE)
) {
  ## 1 Validate arguments

  prompt <- prompt(prompt)

  if (!inherits(llm_provider, "llm_provider"))
    stop("llm_provider must be an object of class 'llm_provider'")

  is_whole_number <- function(x) { x == floor(x) }
  if (!is_whole_number(max_interactions))
    stop("max_interactions should be a whole number.")

  if (!is.logical(verbose))
    stop("verbose should be a logical.")


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
  send_chat <- function(message, role = "user") {
    message <- as.character(message)

    if (verbose) {
      message("--- Sending message to LLM-provider: ---")
      message(message)
    }

    chat_history <<- chat_history |>
      dplyr::bind_rows(create_chat_df(role, message)) # TODO: remove dplyr

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

  prompt_wraps <- get_prompt_wraps_ordered(prompt) |> rev() # In reverse order
  # (Tools, then modes, then unspecified prompt_wraps)

  tries <- 0; successful_output <- FALSE
  while (tries < max_interactions & !successful_output) {
    tries <- tries + 1

    for (prompt_wrap in prompt_wraps) {
      role <- "user"

      # Apply extraction function
      if (!is.null(prompt_wrap$extraction_fn)) {
        extraction_function <- prompt_wrap$extraction_fn

        # If extraction function has attribute 'tool_functions':
        tool_called <- FALSE

        if (!is.null(attr(extraction_function, "tool_functions"))) {
          tool_functions <- attr(extraction_function, "tool_functions")
          extraction_result <- extraction_function(response, tool_functions)
          tool_called <- TRUE
        } else {
          extraction_result <- extraction_function(response)
        }

        # If it inherits llm_feedback, send the feedback to the LLM & get new response
        if (inherits(extraction_result, "llm_feedback")) {
          if (tool_called) role <- "system"
          response <- send_chat(extraction_result, role)
          break
        }

        # If no llm_feedback, extraction was succesful
        response <- extraction_result
      }

      # Apply validation function
      if (!is.null(prompt_wrap$validation_fn)) {
        validation_result <- prompt_wrap$validation_fn(response)

        # If it inherits llm_feedback, send the feedback to the LLM & get new response
        if (inherits(validation_result, "llm_feedback")) {
          response <- send_chat(validation_result)
          break
        }
      }

      # If no errors, break both loops
      successful_output <- TRUE # To break the while-loop
      break # To break the for-loop
    }
  }


  ## 6 Final evaluation

  if (!successful_output)
    stop(paste0(
      "Failed to reach a valid answer after the maximum of ",
      max_interactions, " interactions."
    ))

  return(response)
}
