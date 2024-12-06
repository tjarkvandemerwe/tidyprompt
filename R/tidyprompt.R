#' @title Tidyprompt R6 Class
#'
#' @description
#' A `Tidyprompt` is an object containing a base prompt string and a list
#' of `prompt_wrap` objects. It provides structured methods to modify the prompt,
#' validate responses, and chain modifications, ensuring robust interaction with
#' LLMs.
#'
#' @details
#' This class includes methods for adding prompt wraps, constructing prompt text,
#' and validating the object. It allows chaining modifications to the prompt
#' and validating responses, ensuring a seamless flow in prompt design.
#'
#' @export
#' @family tidyprompt
`tidyprompt-class` <- R6::R6Class(
  "Tidyprompt",
  private = list(
    # Internal validator
    validate_tidyprompt = function() {
      if (!is.character(self$base_prompt) || length(self$base_prompt) != 1) {
        stop("The base prompt must be a single string.", call. = FALSE)
      }
      if (!is.list(self$prompt_wraps)) {
        stop("$prompt_wraps must be a list.", call. = FALSE)
      }
      if (length(self$prompt_wraps) > 0 &&
          !all(sapply(self$prompt_wraps, function(x) inherits(x, "prompt_wrap")))) {
        stop("All elements of $prompt_wraps must be of class `prompt_wrap`.", call. = FALSE)
      }
    }
  ),
  public = list(
    #' @field base_prompt The base prompt string
    base_prompt = NULL,
    #' @field prompt_wraps A list of prompt_wrap objects
    prompt_wraps = list(),
    #' @field system_prompt A system prompt string
    system_prompt = NULL,

    #' @description
    #' Initialize a Tidyprompt object.
    #'
    #' @param input A character string representing the base prompt.
    #' @return A `Tidyprompt` object.
    initialize = function(input) {
      if (!is.character(input) || length(input) != 1) {
        stop("Input must be a single character string.", call. = FALSE)
      }
      self$base_prompt <- input
      self$prompt_wraps <- list()
      private$validate_tidyprompt()
      invisible(self)
    },

    #' @description
    #' Check if the Tidyprompt object is valid.
    #'
    #' @return `TRUE` if valid, otherwise `FALSE`.
    is_valid = function() {
      tryCatch({
        private$validate_tidyprompt()
        TRUE
      }, error = function(e) FALSE)
    },

    #' @description
    #' Add a `prompt_wrap` object to the Tidyprompt.
    #'
    #' @param prompt_wrap A `prompt_wrap` object.
    #' @return The updated Tidyprompt object (invisible).
    add_prompt_wrap = function(prompt_wrap) {
      if (!inherits(prompt_wrap, "prompt_wrap")) {
        stop("`prompt_wrap` must be of class `prompt_wrap`.", call. = FALSE)
      }

      # Update the environment of functions in the prompt_wrap to include `self`.
      functions_to_update <- c("modify_fn", "extraction_fn", "validation_fn",
                               "handler_fn", "parameter_fn")
      for (fn_name in functions_to_update) {
        if (!is.null(prompt_wrap[[fn_name]]) && is.function(prompt_wrap[[fn_name]])) {
          original_env <- environment(prompt_wrap[[fn_name]])
          new_env <- list2env(list(self = self), parent = original_env)
          environment(prompt_wrap[[fn_name]]) <- new_env
        }
      }

      self$prompt_wraps <- c(self$prompt_wraps, list(prompt_wrap))
      private$validate_tidyprompt()
      invisible(self)
    },

    #' @description
    #' Get the list of prompt wraps.
    #'
    #' @param order The order to return the wraps. Options are:
    #'   - "default": as added.
    #'   - "modification": ordered by modification stages.
    #'   - "evaluation": ordered by evaluation stages.
    #'
    #' @return A list of `prompt_wrap` objects.
    get_prompt_wraps = function(order = c("default", "modification", "evaluation")) {
      private$validate_tidyprompt()
      order <- match.arg(order)
      wraps <- self$prompt_wraps

      if (length(wraps) == 0) return(list())
      if (order == "default") return(wraps)

      # Categorize wraps
      t_unspecified <- wraps[sapply(wraps, function(x) x$type == "unspecified")]
      t_mode       <- wraps[sapply(wraps, function(x) x$type == "mode")]
      t_tool       <- wraps[sapply(wraps, function(x) x$type == "tool")]
      t_break      <- wraps[sapply(wraps, function(x) x$type == "break")]

      if (order == "modification") {
        return(c(t_unspecified, t_break, t_mode, t_tool))
      }
      if (order == "evaluation") {
        return(c(t_tool, t_mode, t_break, t_unspecified))
      }
    },

    #' @description
    #' Construct the complete prompt text.
    #'
    #' @param llm_provider Optional LLM provider for context.
    #' @return A character string representing the constructed prompt text.
    construct_prompt_text = function(llm_provider = NULL) {
      private$validate_tidyprompt()
      prompt_text <- self$base_prompt
      wraps <- self$get_prompt_wraps(order = "modification")

      for (wrap in wraps) {
        if (!is.null(wrap$modify_fn)) {
          prompt_text <- wrap$modify_fn(prompt_text, llm_provider)
        }
      }
      prompt_text
    }
  )
)

#' Create a Tidyprompt object
#'
#' @param input Input to prompt. If a character string is passed,
#' a new prompt object will be created with that character string as the base prompt.
#'
#' @return A Tidyprompt object
#' @export
#' @seealso [prompt_wrap()]
#' @family tidyprompt
tidyprompt <- function(input) {
  if (inherits(input, "Tidyprompt")) {
    if (!input$is_valid()) {
      stop("The provided Tidyprompt object is not valid.", call. = FALSE)
    }
    return(input)
  } else if (is.character(input) && length(input) == 1) {
    return(`tidyprompt-class`$new(input))
  } else {
    stop("Input must be a single character string or a Tidyprompt object.", call. = FALSE)
  }
}

#' Check if object is a Tidyprompt
#'
#' @param x An object to check.
#'
#' @return TRUE if the object is a Tidyprompt, otherwise FALSE.
#' @export
#' @family tidyprompt
is_tidyprompt <- function(x) {
  inherits(x, "Tidyprompt") && x$is_valid()
}



#' Get prompt wraps from a Tidyprompt
#'
#' @param x A Tidyprompt object.
#' @param order The order in which to return the prompt wraps. Options: "default", "modification", "evaluation".
#'
#' @return A list of prompt wraps.
#' @export
#' @family tidyprompt
get_prompt_wraps <- function(x, order = c("default", "modification", "evaluation")) {
  if (!is_tidyprompt(x)) {
    stop("x must be a valid Tidyprompt object.", call. = FALSE)
  }
  x$get_prompt_wraps(order = order)
}

#' Construct prompt text from a Tidyprompt object
#'
#' @param x A Tidyprompt object.
#' @param llm_provider An optional LLM provider object.
#'
#' @return The constructed prompt text.
#' @export
#' @family tidyprompt
construct_prompt_text <- function(x, llm_provider = NULL) {
  if (!is_tidyprompt(x)) {
    stop("x must be a valid Tidyprompt object.", call. = FALSE)
  }
  x$construct_prompt_text(llm_provider)
}
