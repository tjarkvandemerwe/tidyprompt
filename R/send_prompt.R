#' ...
#'
#' @param prompt ...
#' @param llm_provider ...
#' @param system_prompt ...
#' @param tool_functions ...
#' @param extraction_functions ...
#' @param validation_functions ...
#' @param max_retries ...
#' @param verbose ...
#' @param extract_validate_mode ...
#'
#' @return ...
#' @export
send_prompt <- function(
    prompt,
    llm_provider = NULL,
    system_prompt = NULL,
    tool_functions = list(),
    extraction_functions = list(),
    validation_functions = list(),
    max_retries = 10,
    verbose = TRUE,
    extract_validate_mode = c("extraction_then_validation", "wrap_by_wrap")
) {
  ## 1 Validate arguments

  extract_validate_mode <- match.arg(extract_validate_mode)
  prompt <- validate_prompt_list(prompt)


  ## 2 Retrieve prompt evaluation settings

  # Retrieve llm provider (prioritizing function argument over prompt)
  if (is.null(llm_provider))
    llm_provider <- get_llm_provider_from_prompt_list(prompt)
  if (is.null(llm_provider))
    stop("No llm_provider provided and no llm_provider found in prompt list.")

  # Retrieve validators, extractors, tool functions (prioritizing function arguments over prompt)
  if (
    length(extraction_functions) == 0 |
    length(validation_functions) == 0
  )
    extractors_validators <- get_extractors_and_validators_from_prompt_list(prompt)
  if (length(extraction_functions) == 0)
    extraction_functions <- extractors_validators$extractors
  if (length(validation_functions) == 0)
    validation_functions <- extractors_validators$validators

  # Retrieve max_retries (prioritizing function argument over prompt)
  # if (is.null(max_retries))
    # max_retries <- get_max_retries_from_prompt_list(prompt) # TODO: implement function

  # Retrieve verbose setting (prioritizing function argument over prompt)
  # if (is.null(verbose))
    # verbose <- get_verbose_from_prompt_list(prompt) # TODO: implement function

  # Retrieve system prompt
  # if (is.null(system_prompt))
  #   system_prompt <- get_system_prompt_from_prompt_list(prompt) # TODO: implement function


  ## 3 Chat_history & send_chat

  # Create internal chat_history
  chat_history <- data.frame(
    role = character(),
    content = character()
  )
  if (!is.null(system_prompt)) {
    chat_history <- chat_history |>
      dplyr::bind_rows(data.frame(
        role = "system",
        content = system_prompt
      ))
  }

  # Create internal function to send_chat to LLM-provider
  send_chat <- function(message) {
    message <- as.character(message)

    # TODO: logging
    if (verbose) {
      message("--- Sending message to LLM-provider: ---")
      message(message)
    }

    chat_history <<- chat_history |>
      dplyr::bind_rows(data.frame(
        role = "user",
        content = message
      ))

    completion <- llm_provider$complete_chat(chat_history)

    if (verbose) {
      message("--- Received response from LLM-provider: ---")
      message(completion$content)
    }

    chat_history <<- chat_history |>
      dplyr::bind_rows(data.frame(
        role = completion$role,
        content = completion$content
      ))

    return(invisible(completion$content))
  }


  ## 4 Retrieve initial response

  response <- send_chat(prompt |> construct_prompt_text())


  ## 6 Extractors & validators toepassen

  # Eerst alle extractors, dan alle validators
  if (extract_validate_mode == "extraction_then_validation") {
    tries <- 0; successful_output <- FALSE
    while (tries < max_retries & !successful_output) {
      tries <- tries + 1

      # Apply extractor functions
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

  # Wrap by wrap, eerst alle extractors+validators van één wrap, dan de volgende
  if (extract_validate_mode == "wrap_by_wrap") {
    # TODO: implementation
  }


  ## 7 Final evaluation

  if (!successful_output)
    stop("Failed to reach a valid answer after ", max_retries, " tries.")

  return(response)
}

# Code to test extractors/validators
if (FALSE) {

  prompt <- "Hi!" |>
    add_text("Can you please calculate what is 5+5? Write the answer out as a word") |>
    answer_as_integer(add_instruction_to_prompt = FALSE) |>
    set_llm_provider(create_ollama_llm_provider()) |>
    set_mode_chainofthought()
  prompt |> construct_prompt_text()
  prompt |> send_prompt()

  tool_functions <- list(temperature_in_location)


  "Hi!" |>
    send_prompt(
      llm_provider = create_ollama_llm_provider(),
      system_prompt = "You are an assistant who always answers in poems. You are also very angry."
    )

}


