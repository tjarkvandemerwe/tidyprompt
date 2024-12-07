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
      if (!is.list(private$prompt_wraps)) {
        stop("$prompt_wraps must be a list.", call. = FALSE)
      }
      if (length(private$prompt_wraps) > 0 &&
          !all(sapply(private$prompt_wraps, function(x) inherits(x, "prompt_wrap")))) {
        stop("All elements of $prompt_wraps must be of class `prompt_wrap`.", call. = FALSE)
      }
    },

    # A list of prompt_wrap objects
    prompt_wraps = list()
  ),
  public = list(
    #' @field base_prompt The base prompt string. This may be modified
    #' by prompt wraps during [construct_prompt_text()]; the constructed
    #' prompt text will be used as the first message of role 'user'
    #' in the chat history during [send_prompt()]
    base_prompt = NULL,
    #' @field system_prompt A system prompt string. This will be added
    #' at the start of the chat history as role 'system' during
    #' [send_prompt()]
    system_prompt = NULL,

    #' @description
    #' Initialize a Tidyprompt object.
    #'
    #' @param input A character string representing the base prompt
    #' @return A `Tidyprompt` object
    initialize = function(input) {
      if (!is.character(input) || length(input) != 1) {
        stop("Input must be a single character string.", call. = FALSE)
      }
      self$base_prompt <- input
      private$prompt_wraps <- list()
      private$validate_tidyprompt()
      invisible(self)
    },

    #' @description
    #' Check if the Tidyprompt object is valid.
    #'
    #' @return `TRUE` if valid, otherwise `FALSE`
    is_valid = function() {
      tryCatch({
        private$validate_tidyprompt()
        TRUE
      }, error = function(e) FALSE)
    },

    #' @description
    #' Add a `prompt_wrap` object to the Tidyprompt.
    #'
    #' @param prompt_wrap A [prompt_wrap()] object
    #' @return The updated Tidyprompt object (invisible)
    add_prompt_wrap = function(prompt_wrap) {
      if (!inherits(prompt_wrap, "prompt_wrap")) {
        stop("`prompt_wrap` must be of class `prompt_wrap`.", call. = FALSE)
      }
      private$prompt_wraps <- c(private$prompt_wraps, list(prompt_wrap))
      private$validate_tidyprompt()
      invisible(self)
    },

    #' @description
    #' Get the list of prompt wraps.
    #'
    #' @param order The order to return the wraps. Options are:
    #'   - "default": as added to the Tidyprompt
    #'   - "modification": ordered for modification of the base prompt;
    #'   ordered by type: unspecified, mode, tool, break. This is the order
    #'   in which wraps are applied during [construct_prompt_text()]
    #'   - "evaluation": ordered for evaluation of the LLM response;
    #'   ordered by type: tool, mode, break, unspecified. This is the order
    #'   in which wraps are applied to the LLM output during [send_prompt()]
    #'
    #' @return A list of `prompt_wrap` objects.
    get_prompt_wraps = function(order = c("default", "modification", "evaluation")) {
      private$validate_tidyprompt()
      order <- match.arg(order)
      wraps <- private$prompt_wraps

      # Update the environment of functions in the prompt_wrap to include `self`.
      functions_to_update <- c(
        "modify_fn", "extraction_fn", "validation_fn", "handler_fn", "parameter_fn"
      )
      i <- 0
      for (wrap in wraps) {
        i <- i + 1
        for (fn_name in functions_to_update) {
          if (!is.null(wrap[[fn_name]]) && is.function(wrap[[fn_name]])) {
            original_env <- environment(wrap[[fn_name]])
            new_env <- list2env(list(self = self), parent = original_env)
            fn <- wrap[[fn_name]]
            environment(fn) <- new_env
            wraps[[i]][[fn_name]] <- fn
          }
        }
      }; rm(i)

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
    #' @param llm_provider Optional \link{llm_provider-class} object.
    #' This may sometimes affect the prompt text construction
    #' @return A string representing the constructed prompt text
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
#' @param input A Tidyprompt object or a string. If a string is passed,
#' a new prompt object will be created with that as the base prompt
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
#' @param x An object to check
#'
#' @return TRUE if the object is a valid Tidyprompt, otherwise FALSE
#' @export
#' @family tidyprompt
is_tidyprompt <- function(x) {
  inherits(x, "Tidyprompt") && x$is_valid()
}



#' Get prompt wraps from a Tidyprompt
#'
#' @param x A Tidyprompt object
#' @param order The order to return the wraps. Options are:
#'   - "default": as added to the Tidyprompt
#'   - "modification": ordered for modification of the base prompt;
#'   ordered by type: unspecified, mode, tool, break. This is the order
#'   in which wraps are applied during [construct_prompt_text()]
#'   - "evaluation": ordered for evaluation of the LLM response;
#'   ordered by type: tool, mode, break, unspecified. This is the order
#'   in which wraps are applied to the LLM output during [send_prompt()]
#'
#' @return A list of prompt wrap objects (see [prompt_wrap()])
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
#' @param x A Tidyprompt object
#' @param llm_provider An optional \link{llm_provider-class} object.
#' This may sometimes affect the prompt text construction
#'
#' @return The constructed prompt text
#' @export
#' @family tidyprompt
construct_prompt_text <- function(x, llm_provider = NULL) {
  if (!is_tidyprompt(x)) {
    stop("x must be a valid Tidyprompt object.", call. = FALSE)
  }
  x$construct_prompt_text(llm_provider)
}
