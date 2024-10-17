#' Methods to create and manipulate prompt_list objects
#'
#' @param input Input to create_prompt_list. If a prompt_list object is passed,
#' the parameters will be updated. If a character string is passed, a new prompt_list
#' object will be created with that character string as the base prompt.
#' @param parameters Named list of parameters to attach to the prompt_list object
#'
#' @return A prompt_list object (or an error if an unsuitable input is provided)
#' @export
create_prompt_list <- function(input, parameters = list()) {
  if (inherits(input, "prompt_list")) {
    input$set_parameters(parameters)
    return(input)
  }

  UseMethod("create_prompt_list")
}

#' Default method to create a prompt list object
#'
#' Is called when the input is not a character string or a prompt_list object.
#'
#' @param input Input to create_prompt_list
#' @param parameters Named list of parameters
#'
#' @return An error message stating that input type is not suitable
#' @exportS3Method create_prompt_list default
create_prompt_list.default <- function(input, parameters = list()) {
  stop(paste0(
    "Input (the base prompt) to create_prompt_list() must be a character string"
  ))
}

#' Method to create a prompt list object from a character string
#'
#' @param input Input to create_prompt_list; the base prompt
#' @param parameters Named list of parameters
#'
#' @return A prompt_list object
#' @exportS3Method create_prompt_list character
create_prompt_list.character <- function(input, parameters = list()) {
  if (length(input) != 1)
    stop("Input (the base prompt) must be length 1")
  if (length(parameters) > 0 & is.null(names(parameters)))
    stop("parameters must be a named list")

  prompt_list <- list()
  class(prompt_list) <- "prompt_list"

  # Create a new environment for the object
  object_env <- new.env()

  # Helper function to create functions with the shared environment
  create_function <- function(fn) {
    environment(fn) <- object_env
    return(fn)
  }

  ## Get/set base prompt:
  # Get base prompt
  prompt_list$get_base_prompt <- create_function(function() {
    return(input)
  })
  # Set base prompt
  prompt_list$set_base_prompt <- create_function(function(new_prompt) {
    if (length(new_prompt) != 1)
      stop("Character input to set_base_prompt() must be length 1")
    input <<- new_prompt
  })

  ## Get/set parameters:
  # Get parameters attached this llm_provider
  prompt_list$get_parameters <- create_function(function() {
    return(parameters)
  })
  # Set parameters attached this llm_provider
  prompt_list$set_parameters <- create_function(function(new_parameters) {
    if (length(new_parameters) > 0 & is.null(names(new_parameters))) {
      stop("new_parameters must be a named list")
    }
    # Merge new parameters with existing ones
    updated_parameters <- utils::modifyList(parameters, new_parameters)
    for (name in names(updated_parameters)) {
      assign(name, updated_parameters[[name]], envir = object_env)
    }
    parameters <<- updated_parameters
  })

  ## Prompt_wraps associated with this promp_list
  prompt_wraps <- list()
  # Get
  prompt_list$get_prompt_wraps <- create_function(function() {
    return(prompt_wraps)
  })
  # Set
  prompt_list$set_prompt_wraps <- create_function(function(new_prompt_wraps) {
    if (!is.list(new_prompt_wraps))
      stop("Input to set_prompt_wraps() must be a list")
    if (!all(sapply(new_prompt_wraps, inherits, "prompt_wrap")))
      stop("Input to set_prompt_wraps() must be a list of prompt_wrap objects")
    prompt_wraps <<- new_prompt_wraps
  })
  # Append prompt_wrap
  prompt_list$append_prompt_wrap <- create_function(function(prompt_wrap) {
    if (!inherits(prompt_wrap, "prompt_wrap"))
      stop("Input to add_prompt_wrap() must be a prompt_wrap object")
    prompt_wraps <<- c(prompt_wraps, list(prompt_wrap))
  })
  # Reorder
  prompt_list$get_prompt_wraps_ordered <- create_function(function() {
    # Extract types from the prompt list, defaulting to "unspecified" if type is NULL
    types <- sapply(prompt_wraps, function(x) {
      if (!is.null(x$type)) x$type else "unspecified"
    })

    # Reorder the prompt list:
    # 1. Keep "unspecified" and other types at the top
    # 2. Place "mode" below them
    # 3. Place "tool" at the bottom
    reordered_list <- c(
      prompt_wraps[types != "mode" & types != "tool"],   # Non-mode and non-tool items
      prompt_wraps[types == "mode"],                     # Mode items
      prompt_wraps[types == "tool"]                      # Tool items
    )

    return(reordered_list)
  })

  ## Construct prompt text
  prompt_list$construct_prompt_text <- create_function(function() {
    prompt_text <- input
    prompt_wraps <- prompt_list$get_prompt_wraps_ordered()

    if (length(prompt_wraps) > 0) {
      for (i in 1:length(prompt_wraps)) {
        prompt_text <- prompt_wraps[[i]]$modify_fn(
          prompt_text, prompt_wraps[[i]]$modify_fn_args
        )
      }
    }
    return(prompt_text)
  })

  ## Get extractions and validations
  prompt_list$get_extractions_and_validations <- create_function(function() {
    extractions <- list()
    validations <- list()
    prompt_wraps <- prompt_list$get_prompt_wraps_ordered() |> rev() # In reverse order

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
  })

  return(prompt_list)
}
