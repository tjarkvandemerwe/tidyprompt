#' Wrap a prompt with empowering functions
#'
#' This function takes a single string or a \link{tidyprompt-class} object and
#' adds a new prompt wrap to it. A prompt wrap is a set of functions
#' that modify the prompt text, extract a value from the LLM response,
#' and validate the extracted value. The functions are used to ensure
#' that the prompt and LLM response is in the correct format and meets the
#' specified criteria; they may also be used to provide the LLM with feedback
#' or additional information, like the result of a tool call.
#'
#' @details For advanced use, modify_fn, extraction_fn, and validation_fn
#' may take 'llm_provider' (as used with [send_prompt()]) as second argument, and the
#' http_list (a list of all HTTP requests made during send_prompt()) as third argument.
#' This is not required, but can be useful for more complex prompt wraps which
#' require additional information about the LLM provider or the HTTP requests made.
#'
#' @details 'modify_fn', 'extraction_fn', 'validation_fn', 'handler_fn', and 'parameter_fn'
#' may also access the \link{tidyprompt-class} object that they are a part of
#' through 'self$...' which is attached to their environment. For instance,
#' other prompt wraps can be accessed through 'self$get_prompt_wraps()'
#'
#' @param prompt A single string or a \link{tidyprompt-class} object
#' @param modify_fn A function that takes the previous prompt text (as
#' first argument) and returns the new prompt text
#' @param extraction_fn A function that takes the LLM response (as first argument)
#' and attempts to extract a value from it. Upon succesful extraction, the function
#' should return the extracted value. If the extraction fails, the function should
#' return a [llm_feedback()] message to initiate a retry. A [llm_break()] can be
#' returned to break the extraction and validation loop, ending [send_prompt()]
#' @param validation_fn A function that takes the (extracted) LLM response
#' (as first argument) and attempts to validate it. Upon succesful validation,
#' the function should return TRUE. If the validation fails, the function should
#' return a [llm_feedback()] message to initiate a retry. A [llm_break()] can be
#' returned to break the extraction and validation loop, ending [send_prompt()]
#' object [llm_break()] can be returned to break the extraction and validation loop
#' @param handler_fn A function that takes a 'completion' object (as returned by
#' `llm_provider$complete_chat()`) and a (potentially modified) completion object.
#' This can be used for advanced side effects, like logging, or native tool calling,
#' or keeping track of token usage. See \link{llm_provider-class} for more information;
#' handler_fn is attached to the \link{llm_provider-class} object that is being used.
#' For example usage, see source code of [answer_using_tools()]
#' @param parameter_fn A function that takes the \link{llm_provider-class} object which is being
#' used with [send_prompt()] and returns a named list of parameters to be
#' set in the \link{llm_provider-class} object via `llm_provider$set_parameters()`. This can be
#' used to configure specific parameters of the \link{llm_provider-class} object when evaluating
#' the prompt. For example, [answer_as_json()] may set different parameters for different APIs
#' related to JSON output. This function is typically only used with advanced
#' prompt wraps that require specific settings in the \link{llm_provider-class} object
#' @param type The type of prompt wrap; one of 'unspecified', 'mode', 'tool', or 'break'.
#' Types are used to determine the order in which prompt wraps are applied.
#' When constructing the prompt text, prompt wraps are applied to the base prompt
#' in the following order: 'unspecified', 'break', 'mode', 'tool'. When evaluating
#' the LLM response and applying extraction and validation functions,
#' prompt wraps are applied in the reverse order: 'tool', 'mode', 'break', 'unspecified'.
#' Order among the same type is preserved in the order they were added to the prompt.
#' Example of a tool is [answer_using_tools()]; example of a mode is [answer_by_react()].
#' Example of a break is [quit_if()]. Most other prompt wraps will be 'unspecified',
#' like [answer_as_regex_match()] or [add_text()]
#' @param name An optional name for the prompt wrap. This can be used to identify
#' the prompt wrap in the \link{tidyprompt-class} object
#'
#' @return A \link{tidyprompt-class} object with the [prompt_wrap()] appended to it
#'
#' @export
#'
#' @example inst/examples/prompt_wrap.R
#' @example R/answer_as_integer.R
#'
#' @family prompt_wrap
#' @family pre_built_prompt_wraps
#'
#' @seealso \link{tidyprompt-class} [send_prompt()]
prompt_wrap <- function(
    prompt,
    modify_fn = NULL,
    extraction_fn = NULL,
    validation_fn = NULL,
    handler_fn = NULL,
    parameter_fn = NULL,
    type = c("unspecified", "mode", "tool", "break"),
    name = NULL
) {
  UseMethod("prompt_wrap")
}



#' [prompt_wrap()] method for when a \link{tidyprompt-class} object is supplied
#'
#' Calls the internal function to append the prompt wrap.
#'
#' @param prompt A single string or a \link{tidyprompt-class} object
#' @param ... Additional arguments
#'
#' @return A \link{tidyprompt-class} object with the [prompt_wrap()] appended to it
#'
#' @exportS3Method prompt_wrap tidyprompt
#' @keywords internal
prompt_wrap.tidyprompt <- function(prompt, ...) {
  prompt_wrap_internal(prompt, ...)
}



#' Default method for [prompt_wrap()]
#'
#' Attempts to create a \link{tidyprompt-class} object from whatever is passed as 'prompt';
#' then calls the internal function to append the [prompt_wrap()].
#'
#' @param prompt A single string or a \link{tidyprompt-class} object
#' @param ... Additional arguments
#'
#' @return A \link{tidyprompt-class} object with the [prompt_wrap()] appended to it
#'
#' @exportS3Method prompt_wrap default
#' @keywords internal
prompt_wrap.default <- function(prompt, ...) {
  prompt <- tidyprompt(prompt)
  prompt_wrap_internal(prompt, ...)
}



#' Internal function to append a [prompt_wrap()] to a \link{tidyprompt-class} object
#'
#' @param prompt See [prompt_wrap()]
#' @param modify_fn See [prompt_wrap()]
#' @param extraction_fn See [prompt_wrap()]
#' @param validation_fn See [prompt_wrap()]
#' @param handler_fn See [prompt_wrap()]
#' @param parameter_fn See [prompt_wrap()]
#' @param type See [prompt_wrap()]
#' @param name See [prompt_wrap()]
#'
#' @return A \link{tidyprompt-class} object with the [prompt_wrap()] appended to it
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
    type = c("unspecified", "mode", "tool", "break"),
    name = NULL
) {
  stopifnot(
    is.null(modify_fn) || is.function(modify_fn),
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
      " must be provided"
    ))

  ensure_three_arguments <- function(func) {
    if (is.null(func)) return(NULL)

    # Get the original arguments of the function
    original_args <- formals(func)

    # Check if the function has more than 3 arguments
    if (length(original_args) > 3) {
      stop(paste0(
        "Function should take at most 3 arguments:",
        " (1) the LLM response, (2) the http_list, and (3) the llm_provider.",
        " Other variables may be accessed from the parent environment and ",
        " do not need to be passed as arguments to the function"
      ))
    }

    # Define the required arguments in the desired order
    required_args <- c("llm_response", "llm_provider", "http_list")

    # Find missing arguments from required_args
    missing_args <- setdiff(required_args, names(original_args))

    # Combine original arguments with missing arguments
    combined_args <- c(
      original_args,
      structure(rep(list(quote(expr = NULL)), length(missing_args)), names = missing_args)
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
  modify_fn <- ensure_three_arguments(modify_fn)
  validation_fn <- ensure_three_arguments(validation_fn)
  extraction_fn <- ensure_three_arguments(extraction_fn)

  if (!is.null(parameter_fn) && length(formals(parameter_fn)) != 1) {
    stop(paste0(
      "Parameter_fn should be a function that takes one argument,",
      " which is the llm_provider"
    ))
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
      parameter_fn = parameter_fn,
      name = name
    ),
    class = "prompt_wrap"
  )

  # Append to prompt
  prompt$add_prompt_wrap(prompt_wrap)

  return(prompt)
}
