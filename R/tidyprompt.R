#' Methods to create and manipulate prompt objects
#'
#' @param input Input to prompt. If a character string is passed,
#' a new prompt object will be created with that character string as the base prompt.
#'
#' @return A prompt object (or an error if an unsuitable input is provided)
#' @export
tidyprompt <- function(input) {
  UseMethod("tidyprompt")
}



#' Default method to create a tidyprompt object
#'
#' Is called when the input is not a character string or a tidyprompt object.
#'
#' @param input Input to create_tidyprompt
#'
#' @return An error message stating that input type is not suitable
#' @exportS3Method tidyprompt default
tidyprompt.default <- function(input) {
  stop(paste0(
    "Input (the base prompt) to tidyprompt() must be a character string"
  ))
}



#' Method to create a tidyprompt object from a character string
#'
#' @param input Input to create_tidyprompt; the base prompt
#'
#' @return A tidyprompt object
#' @exportS3Method tidyprompt character
tidyprompt.character <- function(input) {
  if (length(input) != 1)
    stop("Input (the base prompt) must be length 1")

  tidyprompt <- structure(
    list(
      base_prompt = input,
      prompt_wraps = list()
    ),
    class = "tidyprompt"
  )

  return(tidyprompt)
}



#' Validate and return tidyprompt
#'
#' @param input A tidyprompt object
#'
#' @return A validated tidyprompt object
#' @exportS3Method tidyprompt tidyprompt
tidyprompt.tidyprompt <- function(input) {
  validate_tidyprompt(input)
  return(input)
}



#' Validate tidyprompt
#'
#' @param tidyprompt A tidyprompt object
#'
#' @return TRUE if the tidyprompt is valid, otherwise an error is thrown
#' @export
validate_tidyprompt <- function(tidyprompt) {
  if (!inherits(tidyprompt, "tidyprompt"))
    stop("Tidyprompt is not of class 'tidyprompt'")

  if (!"base_prompt" %in% names(tidyprompt))
    stop("The tidyprompt object must have a base prompt")

  if (
    !is.character(tidyprompt$base_prompt) |
    tidyprompt$base_prompt |> length() != 1
  )
    stop("The base prompt must be a single character string")

  if ("prompt_wraps" %in% names(tidyprompt)) {
    if (!is.list(tidyprompt$prompt_wraps))
      stop("The prompt_wraps must be a list")

    if (!all(sapply(tidyprompt$prompt_wraps, function(x) inherits(x, "prompt_wrap"))))
      stop(paste0(
        "All elements of prompt_wraps must be of class 'prompt_wrap'.",
        " Create a prompt_wrap object with the prompt_wrap() function."
      ))
  }

  return(invisible(TRUE))
}



#' Check if object is a valid tidyprompt
#'
#' @param tidyprompt A tidyprompt object
#'
#' @return TRUE if the object is a valid tidyprompt, otherwise FALSE
#' @export
is_tidyprompt <- function(tidyprompt) {
  tryCatch({
    validate_tidyprompt(tidyprompt)
  }, error = function(e) {
    FALSE
  })
}



#' Get base prompt from tidyprompt
#'
#' @param tidyprompt A tidyprompt object
#'
#' @return The base prompt from the tidyprompt
#' @export
get_base_prompt <- function(tidyprompt) {
  tidyprompt <- validate_tidyprompt(tidyprompt)

  base_prompt <- tidyprompt$base_prompt

  return(base_prompt)
}



#' Get prompt wraps from tidyprompt
#'
#' @param tidyprompt A tidyprompt object
#'
#' @return A list of prompt wraps from the tidyprompt
#' @export
get_prompt_wraps <- function(tidyprompt) {
  tidyprompt <- validate_tidyprompt(tidyprompt)
  return(tidyprompt$prompt_wraps)
}



#' Extract only prompt wraps and reorder them in order of operations
#'
#' This function extracts only prompt wraps from a tidyprompt object and reorders them.
#' The order of operations is as follows:
#'   1. "Unspecified"
#'   2. "Mode"
#'   3. "Tool"
#'
#' @param tidyprompt A tidyprompt object
#'
#' @return A list of prompt wraps from the tidyprompt, reordered in order of operations
#' @export
get_prompt_wraps_ordered <- function(tidyprompt) {
  tidyprompt <- validate_tidyprompt(tidyprompt)

  prompt_wraps <- get_prompt_wraps(tidyprompt)

  # Extract types from the prompt wraps, defaulting to "unspecified" if type is NULL
  types <- sapply(prompt_wraps, function(x) {
    if (!is.null(x$type)) x$type else "unspecified"
  })

  reordered_prompt_wraps <- c(
    prompt_wraps[types == "unspecified"],
    prompt_wraps[types == "mode"],
    prompt_wraps[types == "tool"]
  )

  if (length(prompt_wraps) != length(reordered_prompt_wraps))
    stop("Some prompt wraps have an invalid type")

  return(reordered_prompt_wraps)
}



#' Construct prompt text from a tidyprompt object
#'
#' @param tidyprompt A tidyprompt object
#'
#' @return The prompt text constructed from the tidyprompt object
#' @export
construct_prompt_text <- function(tidyprompt) {
  tidyprompt <- validate_tidyprompt(tidyprompt)

  prompt_text <- get_base_prompt(tidyprompt)

  prompt_wraps <- get_prompt_wraps_ordered(tidyprompt)

  if (length(prompt_wraps) > 0) {
    for (i in 1:length(prompt_wraps)) {
      prompt_text <- prompt_wraps[[i]]$modify_fn(prompt_text)
    }
  }
  return(prompt_text)
}



#' Get extractions and validations from a tidyprompt
#'
#' @param tidyprompt A tidyprompt object
#'
#' @return A list with two lists: extractions and validations
#' @export
get_extractions_and_validations <- function(tidyprompt) {
  tidyprompt <- validate_tidyprompt(tidyprompt)

  extractions <- list()
  validations <- list()
  prompt_wraps <- get_prompt_wraps_ordered(tidyprompt) |> rev() # In reverse order

  if (length(prompt_wraps) == 0)
    return(list(extractions = extractions, validations = validations))

  for (i in 1:length(prompt_wraps)) {
    wrap <- prompt_wraps[[i]]
    extractions <- c(extractions, wrap$extraction_fn)
    validations <- c(validations, wrap$validation_fn)
  }

  return(list(extractions = extractions, validations = validations))
}
