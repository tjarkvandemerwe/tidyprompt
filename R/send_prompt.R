#' Send a prompt or [tidyprompt()] to a LLM provider
#'
#' This function is responsible for sending strings or [tidyprompt()] objects,
#' including their prompt wraps, to a LLM provider (see [llm_provider()]) for evaluation.
#' The function will interact with the LLM provider until a successful response
#' is received or the maximum number of interactions is reached. The function will
#' apply extraction and validation functions to the LLM response, as specified
#' in the prompt wraps (see [prompt_wrap()]). If the maximum number of interactions
#'
#' @param prompt A string or a [tidyprompt()] object
#' @param llm_provider [llm_provider()] object (default is [llm_provider_ollama()]).
#' This object and its settings will be used to evaluate the prompt. Note that
#' the 'verbose' and 'stream' settings in the LLM provider will be overruled by
#' the 'verbose' and 'stream' arguments in this function when those are not NULL.
#' Furthermore, a [tidyprompt()] object may carry '$parameters' which will
#' be set in the [llm_provider()] using the 'llm_provider$set_parameters()' function
#' @param max_interactions Maximum number of interactions allowed with the
#' LLM provider. Default is 10. If the maximum number of interactions is reached
#' without a successful response, 'NULL' is returned as the response (see return
#' value)
#' @param clean_chat_history If the chat history should be cleaned after each
#' interaction. Cleaning the chat history means that only the
#' first and last message from the user, the last message from the assistant,
#' and all messages from the system are used when requesting a new answer from
#' the LLM; keeping the context window clean may increase the LLM's performance
#' @param verbose If the interaction with the LLM provider should be printed
#' to the console. This will overrule the 'verbose' setting in the LLM provider
#' @param stream If the interaction with the LLM provider should be streamed.
#' This setting will only be used if the LLM provider already has a
#' 'stream' parameter (which indicates there is support for streaming). This
#' setting will overrule the 'stream' setting in the LLM provider
#' @param return_mode One of 'full' or 'only_response'. See return value
#'
#' @return If return mode 'only_response', the function will return only the LLM response
#' after extraction and validation functions have been applied (NULL is returned
#' when unsucessful after the maximum number of interactions).
#' If return mode 'full', the function will return a list with the following elements:
#' 'response' (the LLM response after extraction and validation functions have been applied;
#' NULL is returned when unsucessful after the maximum number of interactions),
#' 'chat_history' (a dataframe with the full chat history which led to the final response),
#' 'chat_history_clean' (a dataframe with the cleaned chat history which led to
#' the final response; here, only the first and last message from the user, the
#' last message from the assistant, and all messages from the system are kept),
#' 'start_time' (the time when the function was called),
#' 'end_time' (the time when the function ended),
#' 'duration_seconds' (the duration of the function in seconds), and
#' 'http_list' (a list with all HTTP requests and full responses made for chat completions).
#' When using 'full' and you want to access a specific element during (base R) piping,
#' you can use the '[extract_from_return_list()]' function to assist in this
#'
#' @export
#' @example inst/examples/send_prompt.R
#'
#' @seealso [tidyprompt()], [prompt_wrap()], [llm_provider()], [llm_provider_ollama()],
#' [llm_provider_openai()], [llm_provider_openrouter()]
#'
#' @family prompt_evaluation
send_prompt <- function(
    prompt,
    llm_provider = llm_provider_ollama(),
    max_interactions = 10,
    clean_chat_history = TRUE,
    verbose = NULL,
    stream = NULL,
    return_mode = c("only_response", "full")
) {
  ## 1 Validate arguments

  prompt <- tidyprompt(prompt)
  return_mode <- match.arg(return_mode)

  stopifnot(
    inherits(llm_provider, "llm_provider"),
    max_interactions > 0, max_interactions == floor(max_interactions),
    is.logical(clean_chat_history),
    is.null(verbose) | is.logical(verbose),
    is.null(stream) | is.logical(stream)
  )

  llm_provider <- llm_provider$clone()
  if (!is.null(verbose))
    llm_provider$verbose <- verbose
  if (
    !is.null(stream)
    & !is.null(llm_provider$parameters$stream) # This means the provider supports streaming
  )
    llm_provider$parameters$stream <- stream
  # Apply parameter_fn's to the llm_provider
  for (prompt_wrap in get_prompt_wraps(prompt, order = "default")) {
    if (!is.null(prompt_wrap$parameter_fn)) {
      parameter_fn <- prompt_wrap$parameter_fn
      llm_provider$set_parameters(parameter_fn(llm_provider))
    }
  }

  if (return_mode == "full") {
    start_time <- Sys.time()
    http_list <- list()
  }


  ## 2 Chat_history & send_chat

  create_chat_df <- function(
    role = character(), content = character(), tool_result = logical()
  ) {
    data.frame(role = role, content = content, tool_result = tool_result)
  }

  # Create internal chat_history for this prompt
  chat_history <- create_chat_df()

  # Add system prompt
  if (!is.null(prompt$system_prompt))
    chat_history <- create_chat_df("system", prompt$system_prompt, FALSE)

  # Create internal function to send_chat to the given LLM-provider
  send_chat <- function(message, role = "user", tool_result = FALSE) {
    message <- as.character(message)
    chat_history <<- rbind(chat_history, create_chat_df(
      role, message, tool_result
    ))

    if (clean_chat_history) {
      cleaned_chat_history <- clean_chat_history_fn(chat_history)
      completion <- llm_provider$complete_chat(cleaned_chat_history)
    } else {
      completion <- llm_provider$complete_chat(chat_history)
    }

    chat_history <<- rbind(chat_history, create_chat_df(
      completion$role, completion$content, FALSE
    ))

    if (return_mode == "full")
      http_list[[length(http_list) + 1]] <<- completion$http

    return(invisible(completion$content))
  }

  # Create internal function which cleans the chat_history
  clean_chat_history_fn <- function(chat_history) {
    # Keep only first and last message from user;
    # keep only last message from assistant;
    # keep all messages from system;
    # keep all tool_result messages
    user_rows <- which(chat_history$role == "user")
    assistant_rows <- which(chat_history$role == "assistant")
    system_rows <- which(chat_history$role == "system")
    tool_result_rows <- which(chat_history$tool_result)

    keep_rows <- c(
      system_rows,
      user_rows[c(1, length(user_rows))],
      utils::tail(assistant_rows, 1),
      tool_result_rows
    )

    # Subset the dataframe with these rows
    cleaned_chat_history <- chat_history[sort(unique(keep_rows)), ]
    # (sort(unique()) is used to ensure that the rows are in order)

    return(cleaned_chat_history)
  }


  ## 3 Retrieve initial response

  response <- prompt |> construct_prompt_text() |> send_chat()


  ## 4 Apply extractions and validations

  prompt_wraps <- get_prompt_wraps(prompt, order = "evaluation")
  # (Tools, then modes, then unspecified prompt_wraps)

  tries <- 0; success <- FALSE
  while (tries < max_interactions & !success) {
    tries <- tries + 1

    if (length(prompt_wraps) == 0)
      success <- TRUE

    any_prompt_wrap_not_done <- FALSE
    llm_break <- FALSE
    for (prompt_wrap in prompt_wraps) {
      role <- "user"

      # Apply extraction function
      if (!is.null(prompt_wrap$extraction_fn)) {
        extraction_function <- prompt_wrap$extraction_fn

        # Set the environment of the extraction function, if an
        #   environment was given as attribute. At the moment, this is used
        #   for passing 'tool_functions' from add_tools() to execution here
        if (!is.null(attr(extraction_function, "environment"))) {
          environment <- attr(extraction_function, "environment")
          environment(extraction_function) <- environment
        }
        extraction_result <- extraction_function(response)

        # If it inherits llm_feedback,
        #   send the feedback to the LLM & get new response
        if (
          inherits(extraction_result, "llm_feedback")
          || inherits(extraction_result, "llm_feedback_tool_result")
        ) {
          if (inherits(extraction_result, "llm_feedback_tool_result")) {
            # This ensures tool results are not filtered out when cleaning
            #   the context window in send_prompt()
            response <- send_chat(extraction_result, role, tool_result = TRUE)
          } else {
            response <- send_chat(extraction_result, role, tool_result = FALSE)
          }
          any_prompt_wrap_not_done <- TRUE; break
        }

        if (inherits(extraction_result, "llm_break")) {
          if (!extraction_result$success) {
            any_prompt_wrap_not_done <- TRUE # Will result in no success
          } else {
            any_prompt_wrap_not_done <- FALSE # Will result in success
          }
          response <- extraction_result$object_to_return
          llm_break <- TRUE
          break
        }

        # If no llm_feedback or break, extraction was succesful
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

        if (inherits(validation_result, "llm_break")) {
          if (!validation_result$success) {
            any_prompt_wrap_not_done <- TRUE # Will result in no success
          } else {
            any_prompt_wrap_not_done <- FALSE # Will result in success
          }
          response <- validation_result$object_to_return
          llm_break <- TRUE
          break
        }
      }
    }

    if (!any_prompt_wrap_not_done)
      success <- TRUE

    if (llm_break)
      break
  }


  ## 5 Final evaluation

  if (!success) {
    warning(paste0(
      "Failed to reach a valid answer after the maximum of ",
      max_interactions, " interactions"
    ))

    response <- NULL
  }

  if (return_mode == "only_response")
    return(response)

  if (return_mode == "full") {
    return_list <- list()

    return_list$response <- response
    return_list$chat_history <- chat_history
    return_list$chat_history_clean <- clean_chat_history_fn(chat_history)
    return_list$start_time <- start_time
    return_list$end_time <- Sys.time()
    return_list$duration_seconds <-
      as.numeric(difftime(
        return_list$end_time, return_list$start_time, units = "secs"
      ))
    return_list$http_list <- http_list

    return(return_list)
  }
}
