#' Methods to create and manipulate prompt_list objects
#'
#' @param input Input to prompt_list. If a character string is passed,
#' a new prompt_list object will be created with that character string as the base prompt.
#'
#' @return A prompt_list object (or an error if an unsuitable input is provided)
#' @export
prompt_list <- function(input) {
  UseMethod("prompt_list")
}

#' Default method to create a prompt list object
#'
#' Is called when the input is not a character string or a prompt_list object.
#'
#' @param input Input to create_prompt_list
#'
#' @return An error message stating that input type is not suitable
#' @exportS3Method prompt_list default
prompt_list.default <- function(input) {
  stop(paste0(
    "Input (the base prompt) to prompt_list() must be a character string"
  ))
}

#' Method to create a prompt list object from a character string
#'
#' @param input Input to create_prompt_list; the base prompt
#'
#' @return A prompt_list object
#' @exportS3Method prompt_list character
prompt_list.character <- function(input) {
  if (length(input) != 1)
    stop("Input (the base prompt) must be length 1")

  prompt_list <- list(base_prompt = input)
  class(prompt_list) <- "prompt_list"

  return(prompt_list)
}


#' Method to create a prompt_list object from prompt_list
#'
#' @param input Input to create_prompt_list; the base prompt
#'
#' @return A prompt_list object
#' @exportS3Method prompt_list prompt_list
prompt_list.prompt_list <- function(input) {
  return(input)
}


# Get base prompt
get_base_prompt <- function(prompt_list) {
  base_prompt <- prompt_list$base_prompt

  return(base_prompt)
}

# Get prompt wraps
get_prompt_wraps <- function(prompt_list) {
  classes <- lapply(prompt_list, class)
  prompt_wraps <- prompt_list[classes == "prompt_wrap"]

  return(prompt_wraps)
}



# Extract only prompt wraps and reorder them in order of operations
get_prompt_wraps_ordered <- function(prompt_list) {
  prompt_list <- get_prompt_wraps(prompt_list)

  # Extract types from the prompt list, defaulting to "unspecified" if type is NULL
  types <- sapply(prompt_list, function(x) {
    if (!is.null(x$type)) x$type else "unspecified"
  })

  # Reorder the prompt list:
  # 1. Keep "unspecified" and other types at the top
  # 2. Place "mode" below them
  # 3. Place "tool" at the bottom
  reordered_list <- c(
    prompt_list[types != "mode" & types != "tool"],   # Non-mode and non-tool items
    prompt_list[types == "mode"],                     # Mode items
    prompt_list[types == "tool"]                      # Tool items
  )

  return(reordered_list)
}


# Construct prompt text
construct_prompt_text <- function(prompt_list) {

  prompt_text <- get_base_prompt(prompt_list)

  prompt_wraps <- get_prompt_wraps_ordered(prompt_list)

  if (length(prompt_wraps) > 0) {
    for (i in 1:length(prompt_wraps)) {
      prompt_text <- prompt_wraps[[i]]$modify_fn(prompt_text)
    }
  }
  return(prompt_text)
}

# Get extractions and validations
get_extractions_and_validations <- function(prompt_list) {
  extractions <- list()
  validations <- list()
  prompt_wraps <- get_prompt_wraps_ordered(prompt_list) |> rev() # In reverse order

  if (length(prompt_wraps) == 0)
    return(list(extractions = extractions, validations = validations))

  for (i in 1:length(prompt_wraps)) {
    if (length(prompt_wraps[[i]]$extraction_functions) > 0) {
      for (j in 1:length(prompt_wraps[[i]]$extraction_functions)) {
        extractions <- c(extractions, prompt_wraps[[i]]$extraction_functions[[j]])
      }
    }
    if (length(prompt_wraps[[i]]$validation_functions) > 0) {
      for (j in 1:length(prompt_wraps[[i]]$validation_functions)) {
        validations <- c(validations, prompt_wraps[[i]]$validation_functions[[j]])
      }
    }
  }

  return(list(extractions = extractions, validations = validations))
}
