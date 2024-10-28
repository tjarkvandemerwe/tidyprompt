#' Send a prompt to a LLM provider
#'
#' @param prompt A prompt object or a single string
#' @param llm_provider 'llm_provider' object (default is 'ollama')
#' @param max_interactions Maximum number of interactions before stopping
#' @param clean_chat_history If the chat history should be cleaned after each
#' interaction. Default is TRUE. Cleaning the chat history means that only the
#' first and last message from the user, the last message from the assistant,
#' and all messages from the system are used when requesting a new answer from
#' the LLM; keeping the context window clean may increase the LLM's performance.
#' @param verbose If the interaction with the LLM provider should be printed
#' to the console. Default is TRUE.
#' @param stream If the interaction with the LLM provider should be streamed.
#' Default is TRUE. This setting only be used if the LLM provider already has a
#' 'stream' parameter (which indicates there is support for streaming).
#' @param return_mode One of 'full' or 'only_response'. If 'full', the function
#' will return a list with the following elements: 'success', 'response' (if
#' successful), 'failed_response' (if unsuccessful), 'chat_history', 'start_time',
#' 'end_time', and 'duration_seconds'. If 'only_response', the function will only
#' return the response (or NULL if unsuccessful).
#'
#' @return ...
#' @export
send_prompt <- function(
    prompt,
    llm_provider = create_ollama_llm_provider(),
    max_interactions = 10,
    clean_chat_history = TRUE,
    verbose = getOption("tidyprompt.verbose", TRUE),
    stream = TRUE,
    return_mode = c("full", "only_response")
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
  llm_provider$set_verbose(verbose)

  if (!is.logical(stream))
    stop("stream should be a logical.")
  parameters <- llm_provider$get_parameters()
  if (!is.null(parameters$stream))
    llm_provider$set_parameters(list(stream = stream))

  return_mode <- match.arg(return_mode)
  if (return_mode == "full")
    start_time <- Sys.time()


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
    chat_history <<- rbind(chat_history, create_chat_df(role, message))

    if (clean_chat_history) {
      # Keep only first and last message from user;
      # keep only last message from assistant;
      # keep all messages from system
      user_rows <- which(chat_history$role == "user")
      assistant_rows <- which(chat_history$role == "assistant")
      system_rows <- which(chat_history$role == "system")

      keep_rows <- c(
        system_rows,
        user_rows[c(1, length(user_rows))],
        tail(assistant_rows, 1)
      )

      # Subset the dataframe with these rows
      cleaned_chat_history <- chat_history[sort(unique(keep_rows)), ]
      # (sort(unique()) is used to ensure that the rows are in order)

      completion <- llm_provider$complete_chat(cleaned_chat_history)
    } else {
      completion <- llm_provider$complete_chat(chat_history)
    }
    chat_history <<- rbind(chat_history, create_chat_df(completion$role, completion$content))

    return(invisible(completion$content))
  }


  ## 4 Retrieve initial response

  response <- prompt |> construct_prompt_text() |> send_chat()


  ## 5 Apply extractions and validations

  prompt_wraps <- get_prompt_wraps_ordered(prompt) |> rev() # In reverse order
  # (Tools, then modes, then unspecified prompt_wraps)

  tries <- 0; success <- FALSE
  while (tries < max_interactions & !success) {
    tries <- tries + 1

    if (length(prompt_wraps) == 0)
      success <- TRUE

    any_prompt_wrap_not_done <- FALSE
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
          any_prompt_wrap_not_done <- TRUE; break
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
          any_prompt_wrap_not_done <- TRUE; break
        }
      }
    }

    if (!any_prompt_wrap_not_done)
      success <- TRUE
  }


  ## 6 Final evaluation

  if (!success) {
    warning(paste0(
      "Failed to reach a valid answer after the maximum of ",
      max_interactions, " interactions."
    ))

    failed_response <- response
    response <- NULL
  }

  if (return_mode == "only_response")
    return(response)

  if (return_mode == "full") {
    return_list <- list()
    return_list$success <- success

    if (success) {
      return_list$response <- response
    } else {
      return_list$failed_response <- failed_response
    }

    return_list$chat_history <- chat_history

    return_list$start_time <- start_time
    return_list$end_time <- Sys.time()
    return_list$duration_seconds <-
      as.numeric(difftime(
        return_list$end_time, return_list$start_time, units = "secs"
      ))

    return(return_list)
  }
}
