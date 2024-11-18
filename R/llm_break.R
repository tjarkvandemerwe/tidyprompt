#' Create an `llm_break` object
#'
#' This object is used to break a extraction and validation loop in a [prompt_wrap()]
#' evaluated by [send_prompt()]. When an extraction or validation function returns
#' this object, the loop will be broken and no further extraction or validation
#' functions are applied; instead, [send_prompt()] will be able to return
#' the result at that point. This may be useful in scenarios where
#' it is determined the LLM is unable to provide a response to a prompt.
#'
#' @param object_to_return The object to return as the response result
#' from [send_prompt()] when this object is returned from an extraction or
#' validation function.
#' @param success A logical indicating whether the [send_prompt()] loop break
#' should nonetheless be considered as a successful completion of the
#' extraction and validation process.
#'  If `FALSE`, the `object_to_return` must
#' be `NULL` (as the response result of [send_prompt()] will always be 'NULL'
#' when the evaluation was unsuccessful); if `FALSE`, [send_prompt()] will also
#' print a warning about the unsuccessful evaluation.
#'  If `TRUE`, the `object_to_return`
#' will be returned as the response result of [send_prompt()] (and [send_prompt()])
#' will print no warning about unsuccessful evaluation).
#'
#' @return An object of class "llm_break"
#'
#' @export
#' @example inst/examples/llm_break.R
#'
#' @family prompt_wrap_creation
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

  return(structure(
    list(
      object_to_return = object_to_return,
      success = success
    ),
    class = "llm_break"
  ))
}
