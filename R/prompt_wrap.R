#' Wrap a prompt with empowering functions
#'
#' This function takes a single string or a [tidyprompt()] object and
#' adds a new prompt wrap to it. A prompt wrap is a set of functions
#' that modify the prompt text, extract a value from the LLM response,
#' and validate the extracted value. The functions are used to ensure
#' that the prompt and LLM response is in the correct format and meets the
#' specified criteria.
#'
#' @param prompt A single string or a [tidyprompt()] object
#' @param modify_fn A function that takes the previous prompt text (as
#' first argument) and returns the new prompt text
#' @param extraction_fn A function that takes the LLM response (as first argument)
#' and attempts to extract a value from it.Upon succesful extraction, the function
#' should return the extracted value. If the extraction fails, the function should
#' return a [llm_feedback()] message which will be sent back to the LLM
#' @param validation_fn A function that takes the (extracted) LLM response
#' (as first argument) and attempts to validate it. Upon succesful validation,
#' the function should return TRUE. If the validation fails, the function should
#' return a [llm_feedback()] message which will be sent back to the LLM
#' @param type The type of prompt wrap; one of 'unspecified', 'mode', 'tool', or 'break'.
#' Types are used to determine the order in which prompt wraps are applied.
#' When constructing the prompt text, prompt wraps are applied to the base prompt
#' in the following order: 'unspecified', 'break', 'mode', 'tool'. When evaluating
#' the LLM response and applying extraction and validation functions,
#' prompt wraps are applied in the reverse order: 'tool', 'mode', 'break', 'unspecified'.
#' Example of a tool is [add_tools()]; example of a mode is [answer_by_react()].
#' Example of a break is [quit_if()]. Most other prompt wraps will be 'unspecified',
#' like [answer_as_regex()] or [add_text()]
#'
#' @return A [tidyprompt()] object with the [prompt_wrap()] appended to it
#'
#' @export
#'
#' @example R/answer_as_integer.R
#'
#' @family prompt_wrap
#' @family pre_built_prompt_wraps
#'
#' @seealso [tidyprompt()] [send_prompt()]
prompt_wrap <- function(
    prompt,
    modify_fn = NULL,
    extraction_fn = NULL,
    validation_fn = NULL,
    type = c("unspecified", "mode", "tool")
) {
  UseMethod("prompt_wrap")
}



#' [prompt_wrap()] method for when a [tidyprompt()] object is supplied
#'
#' Calls the internal function to append the prompt wrap.
#'
#' @param prompt A single string or a [tidyprompt()] object
#' @param ... Additional arguments
#'
#' @return A [tidyprompt()] object with the [prompt_wrap()] appended to it
#'
#' @exportS3Method prompt_wrap tidyprompt
#' @keywords internal
prompt_wrap.tidyprompt <- function(prompt, ...) {
  prompt_wrap_internal(prompt, ...)
}



#' Default method for [prompt_wrap()]
#'
#' Attempts to create a [tidyprompt()] object from whatever is passed as 'prompt';
#' then calls the internal function to append the [prompt_wrap()].
#'
#' @param prompt A single string or a [tidyprompt()] object
#' @param ... Additional arguments
#'
#' @return A [tidyprompt()] object with the [prompt_wrap()] appended to it
#'
#' @exportS3Method prompt_wrap default
#' @keywords internal
prompt_wrap.default <- function(prompt, ...) {
  prompt <- tidyprompt(prompt)
  prompt_wrap_internal(prompt, ...)
}



#' Internal function to append a [prompt_wrap()] to a [tidyprompt()] object
#'
#' @param prompt See [prompt_wrap()]
#' @param modify_fn See [prompt_wrap()]
#' @param extraction_fn See [prompt_wrap()]
#' @param validation_fn See [prompt_wrap()]
#' @param type See [prompt_wrap()]
#'
#' @return A [tidyprompt()] object with the [prompt_wrap()] appended to it
#'
#' @noRd
#' @keywords internal
prompt_wrap_internal <- function(
    prompt,
    modify_fn = NULL,
    extraction_fn = NULL,
    validation_fn = NULL,
    type = c("unspecified", "mode", "tool", "break")
) {
  if (!is.null(modify_fn)) {
    if (
      !is.function(modify_fn)
      || length(formals(modify_fn)) != 1
    ) {
      stop(paste0(
        "Modify_fn should be a function that takes one argument,",
        " which is the previous prompt text.",
        "(Other arguments will be taken from the parent environment and do not need",
        " to be passed as arguments to the function)."
      ))
    }
  }

  if (!is.null(validation_fn) && !is.function(validation_fn))
    stop("Validation_fn should be a function.")

  if (!is.null(extraction_fn) && !is.function(extraction_fn))
    stop("Extraction_fn should be a function.")

  if (all(sapply(list(modify_fn, extraction_fn, validation_fn), is.null)))
    stop("At least one of modify_fn, extraction_fn, or validation_fn must be provided.")

  type <- match.arg(type)

  # Create prompt wrap
  prompt_wrap <- structure(
    list(
      type = type,
      modify_fn = modify_fn,
      extraction_fn = extraction_fn,
      validation_fn = validation_fn
    ),
    class = "prompt_wrap"
  )

  # Append to prompt
  prompt$prompt_wraps[[length(prompt$prompt_wraps) + 1]] <- prompt_wrap

  return(prompt)
}
