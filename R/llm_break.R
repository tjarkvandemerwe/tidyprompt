#' Create an `llm_break` object
#'
#' This object is used to break a extraction and validation loop defined in a [prompt_wrap()]
#' as evaluated by [send_prompt()]. When an extraction or validation function returns
#' this object, the loop will be broken and no further extraction or validation
#' functions are applied; instead, [send_prompt()] will be able to return
#' the result at that point. This may be useful in scenarios where
#' it is determined the LLM is unable to provide a response to a prompt.
#'
#' @param object_to_return The object to return as the response result
#' from [send_prompt()] when this object is returned from an extraction or
#' validation function
#' @param success A logical indicating whether the [send_prompt()] loop break
#' should nonetheless be considered as a successful completion of the
#' extraction and validation process.
#'  If `FALSE`, the `object_to_return` must
#' be `NULL` (as the response result of [send_prompt()] will always be 'NULL'
#' when the evaluation was unsuccessful); if `FALSE`, [send_prompt()] will also
#' print a warning about the unsuccessful evaluation.
#'  If `TRUE`, the `object_to_return`
#' will be returned as the response result of [send_prompt()] (and [send_prompt()])
#' will print no warning about unsuccessful evaluation)
#'
#' @return An list of class "llm_break" containing the object to return and
#' a logical indicating whether the evaluation was successful
#'
#' @export
#'
#' @example inst/examples/llm_break.R
#'
#' @family prompt_wrap
#' @family prompt_evaluation
#' @seealso [llm_feedback()]
llm_break <- function(
  object_to_return = NULL,
  success = FALSE
) {
  if (!is.logical(success))
    stop("The 'success' argument must be of type logical.")
  if (!success & !is.null(object_to_return)) {
    object_to_return <- NULL
    warning("If 'success' is FALSE, 'object_to_return' is set to NULL")
  }

  return(
    structure(
      list(
        object_to_return = object_to_return,
        success = success
      ),
      class = "llm_break"
    )
  )
}

#' @title Create an `llm_break_soft` object
#'
#' @description
#' This object is used to break a extraction and validation loop defined in a [prompt_wrap()],
#' as evaluated by [send_prompt()]. When an extraction or validation function returns
#' this object, it will prevent any future interactions with the LLM provider for the
#' current prompt. Remaining extraction and validation functions will still be applied
#' and it will still be possible to pass these with the current response from the LLM
#' provider; only, no more new tries will be made if the current response is not
#' satisfactory.
#'
#' This is useful when, e.g., the token limit for the LLM provider has been reached,
#' but the final response that we got may still be satisfactory. In this case,
#' `llm_break()` cannot be used, as it would instantly return the current response as the final
#' result, which is not what we want. Instead, `llm_break_soft()` can be used to
#' prevent any further interactions with the LLM provider, but still allow the
#' remaining extraction and validation functions to be applied (and have those decide
#' the success of the current response).
#'
#' @param object_to_return The object to return as the response result
#' from [send_prompt()] when this object is returned from an extraction or
#' validation function
#'
#' @return An list of class "llm_break_soft" containing the object to return
#' @export llm_break_soft
#'
#' @example inst/examples/llm_break_soft.R
llm_break_soft <- function(
  object_to_return = NULL
) {
  return(
    structure(
      class = "llm_break_soft",
      list(
        object_to_return = object_to_return
      )
    )
  )
}
