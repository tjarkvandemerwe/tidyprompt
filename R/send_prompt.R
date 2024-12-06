#' Send a prompt to a LLM provider
#'
#' This function is responsible for sending prompts to a LLM provider for evaluation.
#' The function will interact with the LLM provider until a successful response
#' is received or the maximum number of interactions is reached. The function will
#' apply extraction and validation functions to the LLM response, as specified
#' in the prompt wraps (see [prompt_wrap()]). If the maximum number of interactions
#'
#' @param prompt A string or a \link{tidyprompt-class} object
#' @param llm_provider \link{llm_provider-class} object (default is [llm_provider_ollama()]).
#' This object and its settings will be used to evaluate the prompt. Note that
#' the 'verbose' and 'stream' settings in the LLM provider will be overruled by
#' the 'verbose' and 'stream' arguments in this function when those are not NULL.
#' Furthermore, advanced \link{tidyprompt-class} objects may carry '$parameter_fn' functions
#' which can set parameters in the llm_provider object (see [prompt_wrap()] for more information)
#' @param max_interactions Maximum number of interactions allowed with the
#' LLM provider. Default is 10. If the maximum number of interactions is reached
#' without a successful response, 'NULL' is returned as the response (see return
#' value). The first interaction is the initial chat completion
#' @param clean_chat_history If the chat history should be cleaned after each
#' interaction. Cleaning the chat history means that only the
#' first and last message from the user, the last message from the assistant,
#' all messages from the system, and all tool results are kept in a 'clean'
#' chat history. This clean chat history is used when requesting a new chat completion.
#' (i.e., if a LLM repeatedly fails to provide a correct response, only its last failed response
#' will included in the context window). This may increase the LLM performance
#' on the next interaction
#' @param verbose If the interaction with the LLM provider should be printed
#' to the console. This will overrule the 'verbose' setting in the LLM provider
#' @param stream If the interaction with the LLM provider should be streamed.
#' This setting will only be used if the LLM provider already has a
#' 'stream' parameter (which indicates there is support for streaming). Note
#' that when 'verbose' is set to FALSE, the 'stream' setting will be ignored
#' @param return_mode One of 'full' or 'only_response'. See return value
#' @return \itemize{
#'  \item If return mode 'only_response', the function will return only the LLM response
#' after extraction and validation functions have been applied (NULL is returned
#' when unsuccessful after the maximum number of interactions).
#'  \item If return mode 'full', the function will return a list with the following elements:
#'  \itemize{
#'    \item 'response' (the LLM response after extraction and validation functions have been applied;
#'  NULL is returned when unsuccessful after the maximum number of interactions),
#'    \item 'chat_history' (a dataframe with the full chat history which led to the final response),
#'    \item 'chat_history_clean' (a dataframe with the cleaned chat history which led to
#' the final response; here, only the first and last message from the user, the
#' last message from the assistant, and all messages from the system are kept),
#'    \item 'start_time' (the time when the function was called),
#'    \item 'end_time' (the time when the function ended),
#'    \item 'duration_seconds' (the duration of the function in seconds), and
#'    \item 'http_list' (a list with all HTTP responses made during the interactions;
#'    as returned by `llm_provider$complete_chat()`).
#'  }
#' }
#'
#' @export
#'
#' @example inst/examples/send_prompt.R
#'
#' @seealso \link{tidyprompt-class}, [prompt_wrap()], \link{llm_provider-class}, [llm_provider_ollama()],
#' [llm_provider_openai()]
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

  # Basic validation
  prompt <- tidyprompt(prompt)
  return_mode <- match.arg(return_mode)
  stopifnot(
    inherits(llm_provider, "Llm_provider"),
    max_interactions > 0, max_interactions == floor(max_interactions),
    is.logical(clean_chat_history),
    is.null(verbose) | is.logical(verbose),
    is.null(stream) | is.logical(stream)
  )

  # Verify and configure llm_provider
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

  # Initialize variables which keep track of the process
  if (return_mode == "full")
    start_time <- Sys.time()
  http_list <- list()


  ## 2 Chat_history, send_chat, handler_fns

  # Create internal chat_history
  chat_history <- create_chat_df()

  # Add system prompt if available
  if (!is.null(prompt$system_prompt))
    chat_history <- create_chat_df("system", prompt$system_prompt, FALSE)

  # Define variable to keep track of handler_fn breaks
  handler_break <- FALSE

  # Internal function to send chat messages
  send_chat <- function(
    message, role = "user", tool_result = FALSE
  ) {
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

    # Apply 'handler_fn' if available
    for (prompt_wrap in get_prompt_wraps(prompt, order = "default")) {
      if (!is.null(prompt_wrap$handler_fn)) {
        handler_fn <- prompt_wrap$handler_fn
        # Add environment
        if (!is.null(attr(handler_fn, "environment"))) {
          environment <- attr(handler_fn, "environment")
          environment(handler_fn) <- environment
        }

        completion <- handler_fn(completion, llm_provider, http_list)

        if (isTRUE(completion$break_process)) {
          handler_break <<- TRUE
          break
        }
      }
    }

    chat_history <<- rbind(chat_history, create_chat_df(
      completion$role, completion$content, FALSE
    ))

    http_list[[length(http_list) + 1]] <<- completion$http

    return(invisible(completion$content))
  }


  ## 3 Retrieve initial response

  response <- prompt |>
    construct_prompt_text(llm_provider) |>
    send_chat()


  ## 4 Apply extractions and validations

  prompt_wraps <- get_prompt_wraps(prompt, order = "evaluation")
  # (Tools, then modes, then unspecified prompt_wraps)

  tries <- 1; success <- FALSE
  while (tries < max_interactions & !success & !handler_break) {
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
        extraction_result <-
          extraction_function(response, llm_provider, http_list)

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
        validation_result <-
          prompt_wrap$validation_fn(response, llm_provider, http_list)

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
    if (handler_break) {
      warning(paste0(
        "Handler function broke the `send_prompt()` loop",
        " (performed ", tries, " interactions)"
      ))
    } else {
      warning(paste0(
        "Failed to reach a valid answer after the maximum of ",
        max_interactions, " interactions"
      ))
    }

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


# Create internal function which cleans the chat_history



#' Create chat history dataframe
#'
#' @description Internal function used by [send_prompt()] to create a chat history
#' dataframe. This dataframe is used to keep track of the chat history during
#' the interactions with the LLM provider.
#
#' @param role Role of the message (e.g. "user", "assistant", "system")
#' @param content Content of the message
#' @param tool_result Logical indicating whether the message is a tool result.
#' This will ensure it is not removed when cleaning the context window by
#' [clean_chat_history_fn()]
#'
#' @return A dataframe with the chat history
#'
#' @noRd
#' @keywords internal
create_chat_df <- function(
    role = character(), content = character(), tool_result = logical()
) {
  data.frame(role = role, content = content, tool_result = tool_result)
}



#' Clean chat history
#'
#' @description Internal function used by [send_prompt()] to clean the chat history
#' context window after each interaction with the LLM provider.
#'
#' This function will keep only the first and last message from the user, the last
#' message from the assistant, all messages from the system, and all tool_result messages.
#'
#' This may be useful to keep the context window clean and increase the LLM's performance.
#'
#' @param chat_history A dataframe with the full chat history
#'
#' @return A dataframe with the cleaned chat history
#'
#' @noRd
#' @keywords internal
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
