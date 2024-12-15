#' @title Tidyprompt R6 Class
#'
#' @description
#' A [tidyprompt-class] object contains a base prompt and a list
#' of [prompt_wrap()] objects. It provides structured methods to modify the prompt
#' while simultaneously adding logic to extract from and validate the LLM response.
#' Besides a base prompt, a [tidyprompt-class] object may contain a system prompt
#' and a chat history which precede the base prompt.
#'
#' @export
#'
#' @family tidyprompt
`tidyprompt-class` <- R6::R6Class(
  "Tidyprompt",
  private = list(
    # Internal validator
    validate_tidyprompt = function() {
      if (!is.character(self$base_prompt) || length(self$base_prompt) != 1)
        stop("The base prompt must be a single string.", call. = FALSE)

      if (!is.list(private$prompt_wraps))
        stop("$prompt_wraps must be a list.", call. = FALSE)

      if (
        length(private$prompt_wraps) > 0
        && !all(sapply(private$prompt_wraps, function(x) inherits(x, "prompt_wrap")))
      )
        stop("All elements of $prompt_wraps must be of class `prompt_wrap`.", call. = FALSE)

      if (!is.null(private$chat_history)) {
        tryCatch(
          chat_history(private$chat_history),
          error = function(e) {
            stop(paste0(
              "The chat history is not valid.\n",
              "Error in `chat_history(private$chat_history)`:\n", e$message
            ))
          }
        )
      }
    },

    # A list of prompt_wrap objects
    prompt_wraps = list(),

    # A chat history object
    chat_history = NULL
  ),
  public = list(
    #' @field base_prompt
    #' The base prompt string.
    #' The base prompt be modified by prompt wraps during [construct_prompt_text()];
    #' the modified prompt text will be used as the final message of role 'user'
    #' during [send_prompt()]
    base_prompt = NULL,

    #' @field system_prompt
    #' A system prompt string.
    #' This will be added at the start of the chat history as role 'system'
    #' during [send_prompt()]
    system_prompt = NULL,

    #' @description
    #' Initialize a [tidyprompt-class] object
    #'
    #' @details Different types of input are accepted for initialization of
    #' a [tidyprompt-class] object:
    #' \itemize{
    #'  \item A single character string. This will be used as the base prompt
    #'
    #'  \item A dataframe which is a valid chat history (see [chat_history()])
    #'
    #'  \item A list containing a valid chat history under '$chat_history'
    #'  (e.g., a result from [send_prompt()] when using 'return_mode' = "full")
    #'
    #'  \item A [tidyprompt-class] object. This will be checked for validity and, if valid,
    #'   the fields are copied to the object which is returned from this method
    #' }
    #' When passing a dataframe or list with a chat history, the last row of the
    #' chat history must have role 'user'; this row will be used as the base prompt.
    #' If the first row of the chat history has role 'system', it will be used
    #' as the system prompt.
    #'
    #' @param input A string, a chat history, a list containing
    #' a chat history under key '$chat_history', or a [tidyprompt-class] object
    #'
    #' @return A [tidyprompt-class] object
    initialize = function(input) {
      input_must_be <- paste0(
        "Input must be:",
        " a string,",
        " a dataframe which is a valid chat history (see `?chat_history`),",
        " a list containing a valid chat history under key '$chat_history',",
        " or a `tidyprompt-class` object"
      )
      # Turn single string input into base prompt
      if (is.character(input) && length(input) == 1) {
        self$base_prompt <- input
        return(self)
      }

      # Take chat_history from list input
      if (is.list(input) & !is.data.frame(input)) {
        if (is.null(input$chat_history)) {
          stop(input_must_be)
        }
        input <- input$chat_history
      }

      # Take relevant variables from dataframe input
      if (is.data.frame(input)) {
        chat_history <- tryCatch(
          chat_history(input),
          error = function(e) {
            stop(paste0(
              "Input for `tidyprompt-class` is a dataframe, but dataframe is not",
              " a valid chat history.\n",
              "Error in `chat_history(input)`:\n",
              e$message, "\n",
              "(see `?chat_history`)"
            ))
            NULL
          }
        )

        if (is.null(chat_history)) {
          stop(input_must_be)
        }

        # Last row of chat history must be user
        if (tail(chat_history$role, 1) != "user") {
          stop(paste0(
            "The last row of the chat history must have role 'user'.\n",
            "Add a message to the chat history with `chat_history_add_msg()`"
          ))
        }

        # Extract base prompt from chat history
        self$base_prompt <- tail(chat_history$content, 1)
        chat_history <- chat_history[-nrow(chat_history), ]

        # Extract system prompt from chat history
        if (
          nrow(chat_history) > 0
          && head(chat_history$role, 1) == "system"
        ) {
          self$system_prompt <- head(chat_history$content, 1)
          chat_history <- chat_history[-1, ]
        }

        # Add the rest of chat history to field
        private$chat_history <- chat_history

        return(self)
      }

      # Validate a pre-existing tidyprompt object
      if (inherits(input, "Tidyprompt")) {
        if (!input$is_valid()) {
          stop("The input `tidyprompt-class` object is not valid")
        }

        # Copy fields
        self$base_prompt <- input$base_prompt
        self$system_prompt <- input$system_prompt
        private$chat_history <- input$.__enclos_env__$private$chat_history
        private$prompt_wraps <- input$.__enclos_env__$private$prompt_wraps

        return(self)
      }

      stop(input_must_be)
    },

    #' @description
    #' Check if the tidyprompt object is valid.
    #'
    #' @return `TRUE` if valid, otherwise `FALSE`
    is_valid = function() {
      tryCatch({
        private$validate_tidyprompt()
        TRUE
      }, error = function(e) FALSE)
    },

    #' @description
    #' Add a [prompt_wrap()] to the [tidyprompt-class] object.
    #'
    #' @param prompt_wrap A [prompt_wrap()] object
    #'
    #' @return The updated [tidyprompt-class] object
    add_prompt_wrap = function(prompt_wrap) {
      if (!inherits(prompt_wrap, "prompt_wrap")) {
        stop("`prompt_wrap` must be of class `prompt_wrap`", call. = FALSE)
      }
      private$prompt_wraps <- c(private$prompt_wraps, list(prompt_wrap))
      private$validate_tidyprompt()
    },

    #' @description
    #' Get list of [prompt_wrap()] objects from the [tidyprompt-class] object.
    #'
    #' @param order The order to return the wraps. Options are:
    #'   - "default": as originally added to the object
    #'
    #'   - "modification": as ordered for modification of the base prompt;
    #'   ordered by type: check, unspecified, mode, tool, break. This is the order
    #'   in which prompt wraps are applied during [construct_prompt_text()]
    #'
    #'   - "evaluation": ordered for evaluation of the LLM response;
    #'   ordered by type: tool, mode, break, unspecified, check. This is the order
    #'   in which wraps are applied to the LLM output during [send_prompt()]
    #'
    #' @return A list of [prompt_wrap()] objects.
    get_prompt_wraps = function(
      order = c("default", "modification", "evaluation")
    ) {
      private$validate_tidyprompt()
      order <- match.arg(order)
      wraps <- private$prompt_wraps

      # Update the environment of functions in the `prompt_wrap` to include `self`
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
      t_check <- wraps[sapply(wraps, function(x) x$type == "check")]
      t_unspecified <- wraps[sapply(wraps, function(x) x$type == "unspecified")]
      t_mode       <- wraps[sapply(wraps, function(x) x$type == "mode")]
      t_tool       <- wraps[sapply(wraps, function(x) x$type == "tool")]
      t_break      <- wraps[sapply(wraps, function(x) x$type == "break")]

      if (order == "modification") {
        return(c(t_check, t_unspecified, t_break, t_mode, t_tool))
      }
      if (order == "evaluation") {
        return(c(t_tool, t_mode, t_break, t_unspecified, t_check))
      }
    },

    #' @description
    #' Construct the complete prompt text.
    #'
    #' @param llm_provider Optional [llm_provider-class] object.
    #' This may sometimes affect the prompt text construction
    #'
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
    },

    #' @description This function sets the chat history for the tidyprompt object.
    #' The chat history will also set the base prompt and system prompt
    #' (the last message of the chat history should be of role 'user' and
    #' will be used as the base prompt; the first message of the chat history
    #' may be of the role 'system' and will then be used as the system prompt).
    #' This may be useful when one wants to change the base prompt, system prompt,
    #' and chat history of a [tidyprompt-class] object while retaining other fields like
    #' the prompt wraps.
    #'
    #' @param chat_history A valid chat history (see [chat_history()])
    #'
    #' @return The updated [tidyprompt-class] object
    set_chat_history = function(chat_history) {
      chat_history <- chat_history(chat_history)

      # Last row of chat history must be user
      if (tail(chat_history$role, 1) != "user") {
        stop(paste0(
          "The last row of the chat history must have role 'user'.\n",
          "Add a message to the chat history using `chat_history_add_msg()`"
        ))
      }

      self$base_prompt <- tail(chat_history$content, 1)
      # Remove base prompt from chat history
      chat_history <- chat_history[-nrow(chat_history), ]

      # If first row is system message, we will set it as the system prompt
      if (
        nrow(chat_history) > 0
        && head(chat_history$role, 1) == "system"
      ) {
        self$system_prompt <- head(chat_history$content, 1)
        # Remove system prompt from chat history
        chat_history <- chat_history[-1, ]
      }

      private$chat_history <- chat_history
    },

    #' @description This function gets the chat history of the [tidyprompt-class] object.
    #' The chat history is constructed from the base prompt, system prompt, and chat
    #' history field. The returned object will be the chat history
    #' with the system prompt as the first message with role 'system' and the
    #' the base prompt as the last message with role 'user'.
    #'
    #' @return A dataframe containing the chat history
    get_chat_history = function() {
      chat_history_construction <- c(
        role = "system", content = self$system_prompt
      ) |> dplyr::bind_rows(
        private$chat_history
      ) |> dplyr::bind_rows(c(
        role = "user", content = self$construct_prompt_text()
      ))

      # Remove roles with no content
      chat_history_construction <- chat_history_construction |>
        dplyr::filter(.data$content != "" & !is.na(.data$content) & !is.null(.data$content))

      chat_history <- chat_history(chat_history_construction)
      chat_history
    }
  )
)



#' Create a [tidyprompt-class] object
#'
#' This is a wrapper around the [tidyprompt-class] constructor.
#'
#' @details Different types of input are accepted for initialization of
#' a [tidyprompt-class] object:
#' \itemize{
#'  \item A single character string. This will be used as the base prompt
#'
#'  \item A dataframe which is a valid chat history (see [chat_history()])
#'
#'  \item A list containing a valid chat history under '$chat_history'
#'  (e.g., a result from [send_prompt()] when using 'return_mode' = "full")
#'
#'  \item A [tidyprompt-class] object. This will be checked for validity and, if valid,
#'   the fields are copied to the object which is returned from this method
#' }
#' When passing a dataframe or list with a chat history, the last row of the
#' chat history must have role 'user'; this row will be used as the base prompt.
#' If the first row of the chat history has role 'system', it will be used
#' as the system prompt.
#'
#' @param input A string, a chat history, a list containing
#' a chat history under key '$chat_history', or a [tidyprompt-class] object
#'
#' @return A [tidyprompt-class] object
#'
#' @export
#'
#' @family tidyprompt
#' @example inst/examples/tidyprompt.R
tidyprompt <- function(input) {
  `tidyprompt-class`$new(input)
}



#' Check if object is a [tidyprompt-class] object
#'
#' @param x An object to check
#'
#' @return TRUE if the object is a valid [tidyprompt-class] object, otherwise FALSE
#'
#' @export
#'
#' @family tidyprompt
#' @example inst/examples/tidyprompt.R
is_tidyprompt <- function(x) {
  inherits(x, "Tidyprompt") && x$is_valid()
}



#' Get prompt wraps from a [tidyprompt-class] object
#'
#' @param x A [tidyprompt-class] object
#'
#' @param order The order to return the wraps. Options are:
#'   - "default": as originally added to the object
#'
#'   - "modification": as ordered for modification of the base prompt;
#'   ordered by type: check, unspecified, mode, tool, break. This is the order
#'   in which prompt wraps are applied during [construct_prompt_text()]
#'
#'   - "evaluation": ordered for evaluation of the LLM response;
#'   ordered by type: tool, mode, break, unspecified, check. This is the order
#'   in which wraps are applied to the LLM output during [send_prompt()]
#'
#' @return A list of prompt wrap objects (see [prompt_wrap()])
#'
#' @export
#'
#' @family tidyprompt
#' @example inst/examples/tidyprompt.R
get_prompt_wraps <- function(x, order = c("default", "modification", "evaluation")) {
  x <- tidyprompt(x)
  x$get_prompt_wraps(order = order)
}



#' Construct prompt text from a [tidyprompt-class] object
#'
#' @param x A [tidyprompt-class] object
#' @param llm_provider An optional [llm_provider-class] object.
#' This may sometimes affect the prompt text construction
#'
#' @return The constructed prompt text
#'
#' @export
#'
#' @family tidyprompt
#' @example inst/examples/tidyprompt.R
construct_prompt_text <- function(x, llm_provider = NULL) {
  x <- tidyprompt(x)
  x$construct_prompt_text(llm_provider)
}



#' @title
#' Set the chat history for [tidyprompt-class] object
#'
#' @description
#' This function sets the chat history for a [tidyprompt-class] object.
#' The chat history will also set the base prompt and system prompt
#' (the last message of the chat history should be of role 'user' and
#' will be used as the base prompt; the first message of the chat history
#' may be of the role 'system' and will then be used as the system prompt).
#'
#' This may be useful when one wants to change the base prompt, system prompt,
#' and chat history of a [tidyprompt-class] object while retaining other fields like
#' the list of prompt wraps.
#'
#' @param x A [tidyprompt-class] object
#' @param chat_history A valid chat history (see [chat_history()])
#'
#' @return The updated [tidyprompt-class] object
#'
#' @export
#'
#' @family tidyprompt
#' @seealso [chat_history()]
#'
#' @example inst/examples/tidyprompt.R
set_chat_history <- function(x, chat_history) {
  x <- tidyprompt(x)
  x$set_chat_history(chat_history)
}



#' Get the chat history of a [tidyprompt-class] object
#'
#' This function gets the chat history of the [tidyprompt-class] object.
#' The chat history is constructed from the base prompt, system prompt, and chat
#' history field. The returned object will be the chat history
#' with the system prompt as the first message with role 'system' and the
#' the base prompt as the last message with role 'user'.
#'
#' @param x A [tidyprompt-class] object
#'
#' @return A dataframe containing the chat history
#'
#' @export
#'
#' @family tidyprompt
#' @example inst/examples/tidyprompt.R
#'
#' @seealso [chat_history()]
get_chat_history <- function(x) {
  x <- tidyprompt(x)
  x$get_chat_history()
}
