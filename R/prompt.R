#' Methods to create and manipulate prompt objects
#'
#' @param input Input to prompt. If a character string is passed,
#' a new prompt object will be created with that character string as the base prompt.
#'
#' @return A prompt object (or an error if an unsuitable input is provided)
#' @export
prompt <- function(input) {
  UseMethod("prompt")
}



#' Default method to create a prompt object
#'
#' Is called when the input is not a character string or a prompt object.
#'
#' @param input Input to create_prompt
#'
#' @return An error message stating that input type is not suitable
#' @exportS3Method prompt default
prompt.default <- function(input) {
  stop(paste0(
    "Input (the base prompt) to prompt() must be a character string"
  ))
}



#' Method to create a prompt object from a character string
#'
#' @param input Input to create_prompt; the base prompt
#'
#' @return A prompt object
#' @exportS3Method prompt character
prompt.character <- function(input) {
  if (length(input) != 1)
    stop("Input (the base prompt) must be length 1")

  prompt <- structure(
    list(
      base_prompt = input,
      prompt_wraps = list()
    ),
    class = "prompt"
  )

  return(prompt)
}



#' Method to validate a prompt object
#'
#' @param input A prompt object
#'
#' @return A validated prompt object
#' @exportS3Method prompt prompt
prompt.prompt <- function(input) {
  # TODO: write validations

  if (!"base_prompt" %in% names(input))
    stop("The prompt object must have a base prompt")

  if (
    !is.character(input$base_prompt) |
    input$base_prompt |> length() != 1
  )
    stop("The base prompt must be a single character string")

  if ("prompt_wrap" %in% names(input)) {
    if (!is.list(input$prompt_wraps))
      stop("The prompt_wraps must be a list")

    if (!all(sapply(input$prompt_wraps, function(x) inherits(x, "prompt_wrap"))))
      stop(paste0(
        "All elements of prompt_wraps must be of class 'prompt_wrap'.",
        " Create a prompt_wrap object with the prompt_wrap() function."
      ))
  }

  return(input)
}



#' Get base prompt from prompt
#'
#' @param prompt A prompt object
#'
#' @return The base prompt from the prompt
#' @export
get_base_prompt <- function(prompt) {
  prompt <- prompt(prompt)

  base_prompt <- prompt$base_prompt

  return(base_prompt)
}



#' Get prompt wraps from prompt
#'
#' @param prompt A prompt object
#'
#' @return A list of prompt wraps from the prompt
#' @export
get_prompt_wraps <- function(prompt) {
  prompt <- prompt(prompt)
  return(prompt$prompt_wraps)
}



#' Extract only prompt wraps and reorder them in order of operations
#'
#' This function extracts only prompt wraps from a prompt object and reorders them.
#' The order of operations is as follows:
#'   1. "Unspecified"
#'   2. "Mode"
#'   3. "Tool"
#'
#' @param prompt A prompt object
#'
#' @return A list of prompt wraps from the prompt, reordered in order of operations
#' @export
get_prompt_wraps_ordered <- function(prompt) {
  prompt <- prompt(prompt)

  prompt_wraps <- get_prompt_wraps(prompt)

  # Extract types from the prompt, defaulting to "unspecified" if type is NULL
  types <- sapply(prompt_wraps, function(x) {
    if (!is.null(x$type)) x$type else "unspecified"
  })

  # Reorder the prompt wraps:
  # 1. Keep "unspecified" and other types at the top
  # 2. Place "mode" below them
  # 3. Place "tool" at the bottom
  reordered_prompt_wraps <- c(
    prompt_wraps[types != "mode" & types != "tool"],
    prompt_wraps[types == "mode"],
    prompt_wraps[types == "tool"]
  )

  return(reordered_prompt_wraps)
}



#' Construct prompt text from a prompt object
#'
#' @param prompt A prompt object
#'
#' @return The prompt text constructed from the prompt object
#' @export
construct_prompt_text <- function(prompt) {
  prompt <- prompt(prompt)

  prompt_text <- get_base_prompt(prompt)

  prompt_wraps <- get_prompt_wraps_ordered(prompt)

  if (length(prompt_wraps) > 0) {
    for (i in 1:length(prompt_wraps)) {
      prompt_text <- prompt_wraps[[i]]$modify_fn(prompt_text)
    }
  }
  return(prompt_text)
}



#' Get extractions and validations from a prompt
#'
#' @param prompt A prompt object
#'
#' @return A list with two lists: extractions and validations
#' @export
get_extractions_and_validations <- function(prompt) {
  prompt <- prompt(prompt)

  extractions <- list()
  validations <- list()
  prompt_wraps <- get_prompt_wraps_ordered(prompt) |> rev() # In reverse order

  if (length(prompt_wraps) == 0)
    return(list(extractions = extractions, validations = validations))

  for (i in 1:length(prompt_wraps)) {
    wrap <- prompt_wraps[[i]]
    extractions <- c(extractions, wrap$extraction_fn)
    validations <- c(validations, wrap$validation_fn)
  }

  return(list(extractions = extractions, validations = validations))
}
