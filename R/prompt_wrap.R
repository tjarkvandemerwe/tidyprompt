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
#' and attempts to extract a value from it. Upon succesful extraction, the function
#' should return the extracted value. If the extraction fails, the function should
#' return a [llm_feedback()] message which will be sent back to the LLM to
#' initiate a retry. A special object [llm_break()] can be returned to break the
#' extraction and validation loop
#' @param validation_fn A function that takes the (extracted) LLM response
#' (as first argument) and attempts to validate it. Upon succesful validation,
#' the function should return TRUE. If the validation fails, the function should
#' return a [llm_feedback()] message which will be sent back to the LLM. A special
#' object [llm_break()] can be returned to break the extraction and validation loop
#' @param handler_fn A function that takes the LLM response (as first argument)
#' and the 'http_list' (as second argument). Handler functions are applied
#' every time a LLM response is received, before any extraction or validation functions
#' are applied. This can be useful for logging, implementing native tool calling,
#' or other side effects, like keeping track of tokens and exiting the process if required
#' based on some specific criteria. Like extraction functions, handler functions may
#' Like extraction functions, handler functions may return [llm_feedback()], [llm_break()].
#' If the handler function does not return either of those, it must return a list of the LLM
#' response ('$response') and the 'http_list' ('$http_list'). This list will be
#' passed to the extraction and validation functions
#' @param parameter_fn A function that takes the [llm_provider()] which is being
#' used with [send_prompt()] and returns a named list of parameters to be
#' used in the [llm_provider()] when sending the prompt. This function is called once,
#' before any LLM responses are requested. It can be used to set specific parameters
#' of the [llm_provider()] according to its characteristics, like the API type
#' (e.g., [answer_as_json()] may set different parameters for different APIs)
#' @param type The type of prompt wrap; one of 'unspecified', 'mode', 'tool', or 'break'.
#' Types are used to determine the order in which prompt wraps are applied.
#' When constructing the prompt text, prompt wraps are applied to the base prompt
#' in the following order: 'unspecified', 'break', 'mode', 'tool'. When evaluating
#' the LLM response and applying extraction and validation functions,
#' prompt wraps are applied in the reverse order: 'tool', 'mode', 'break', 'unspecified'.
#' Order among the same type is preserved in the order they were added to the prompt.
#' Example of a tool is [add_tools()]; example of a mode is [answer_by_react()].
#' Example of a break is [quit_if()]. Most other prompt wraps will be 'unspecified',
#' like [answer_as_regex()] or [add_text()]
#'
#' @return A [tidyprompt()] object with the [prompt_wrap()] appended to it
#'
#' @export
#'
#' @example inst/examples/prompt_wrap.R
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
    handler_fn = NULL,
    parameter_fn = NULL,
    type = c("unspecified", "mode", "tool", "break")
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
#' @param handler_fn See [prompt_wrap()]
#' @param parameter_fn See [prompt_wrap()]
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
    handler_fn = NULL,
    parameter_fn = NULL,
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

  stopifnot(
    is.null(validation_fn) || is.function(validation_fn),
    is.null(extraction_fn) || is.function(extraction_fn),
    is.null(handler_fn) || is.function(handler_fn),
    is.null(parameter_fn) || is.function(parameter_fn)
  )

  if (all(sapply(list(
    modify_fn, extraction_fn, validation_fn, handler_fn, parameter_fn
  ), is.null)))
    stop(paste0(
      "At least one of modify_fn, extraction_fn, validation_fn, handler_fn, or parameter_fn",
      " must be provided."
    ))

  ensure_three_arguments <- function(func) {
    if (is.null(func)) return(NULL)

    # Get the original arguments of the function
    original_args <- formals(func)

    # Check if the function has more than 3 arguments
    if (length(original_args) > 3) {
      stop(paste0(
        "Function should take at most 3 arguments:",
        " (1) the LLM response, (2) the http_list, and (3) the llm_provider."
      ))
    }

    # Define the required arguments in the desired order
    required_args <- c("llm_response", "http_list", "llm_provider")

    # Find missing arguments from required_args
    missing_args <- setdiff(required_args, names(original_args))

    # Combine original arguments with missing arguments
    combined_args <- c(
      original_args,
      setNames(rep(list(quote(expr = )), length(missing_args)), missing_args)
    )

    # Ensure the required arguments are in order, keeping original names first
    final_args <- c(
      combined_args[names(original_args)],              # Original arguments in original order
      combined_args[setdiff(required_args, names(original_args))] # Add missing required arguments
    )

    # Limit to at most three arguments
    final_args <- final_args[1:3]

    # Update the function's arguments
    formals(func) <- final_args
    func
  }
  validation_fn <- ensure_three_arguments(validation_fn)
  extraction_fn <- ensure_three_arguments(extraction_fn)
  handler_fn <- ensure_three_arguments(handler_fn)

  if (!is.null(parameter_fn)) {
    if (length(formals(parameter_fn)) != 1) {
      stop(paste0(
        "Parameter_fn should be a function that takes one argument,",
        " which is the llm_provider."
      ))
    }
  }

  type <- match.arg(type)

  # Create prompt wrap
  prompt_wrap <- structure(
    list(
      type = type,
      modify_fn = modify_fn,
      extraction_fn = extraction_fn,
      validation_fn = validation_fn,
      handler_fn = handler_fn,
      parameter_fn = parameter_fn
    ),
    class = "prompt_wrap"
  )

  # Append to prompt
  prompt$prompt_wraps[[length(prompt$prompt_wraps) + 1]] <- prompt_wrap

  return(prompt)
}
