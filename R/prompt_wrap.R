#' @title
#' Wrap a prompt with functions for modification and handling the LLM response
#'
#' @description
#' This function takes a single string or a [tidyprompt-class] object and
#'  adds a new prompt wrap to it.
#'
#' A prompt wrap is a set of functions that modify the prompt text,
#'  extract a value from the LLM response, and validate the extracted value.
#'
#' The functions are used to ensure that the prompt and LLM response are in the
#'  correct format and meet the specified criteria; they may also be used to
#'  provide the LLM with feedback or additional information,
#'  like the result of a tool call or some evaluated code.
#'
#' Advanced prompt wraps may also include functions that directly handle
#' the response from a LLM API or configure API parameters.
#'
#' @details
#' For advanced use, modify_fn, extraction_fn, and validation_fn
#'  may take the [llm_provider-class] object (as used with [send_prompt()]) as
#'  second argument, and the 'http_list' (a list of all HTTP requests
#'  and responses made during [send_prompt()]) as third argument.
#' Use of these arguments is not required, but can be useful for more complex
#'  prompt wraps which require additional information about the LLM provider
#'  or requests made so far.
#' The functions (including parameter_fn) also have access to
#'  the object `self` (not a function argument; it is attached to the environment
#'  of the function) which contains the [tidyprompt-class] object that the prompt wrap
#'  is a part of. This can be used to access other prompt wraps, or to access the
#'  prompt text or other information about the prompt. For instance,
#'  other prompt wraps can be accessed through `self$get_prompt_wraps()`.
#'
#' @param prompt A string or a [tidyprompt-class] object
#'
#' @param modify_fn A function that takes the previous prompt text (as
#'  first argument) and returns the new prompt text
#'
#' @param extraction_fn A function that takes the LLM response (as first argument)
#'  and attempts to extract a value from it.
#' Upon succesful extraction, the function should return the extracted value.
#'  If the extraction fails, the function should return a [llm_feedback()] message
#'  to initiate a retry.
#' A [llm_break()] can be returned to break the extraction and validation loop,
#'  ending [send_prompt()]
#'
#' @param validation_fn A function that takes the (extracted) LLM response
#'  (as first argument) and attempts to validate it.
#' Upon succesful validation, the function should return TRUE. If the validation
#'  fails, the function should return a [llm_feedback()] message to initiate a retry.
#' A [llm_break()] can be returned to break the extraction and validation loop,
#'  ending [send_prompt()]
#'
#' @param handler_fn A function that takes a 'completion' object (a result
#'  of a request to a LLM, as returned by `$complete_chat()` of a [llm_provider-class]
#'  object) as first argument and the [llm_provider-class] object as second argument.
#' The function should return a (modified or identical) completion object.
#' This can be used for advanced side effects, like logging, or native tool calling,
#'  or keeping track of token usage. See [llm_provider-class] for more information;
#'  handler_fn is attached to the [llm_provider-class] object that is being used.
#' For example usage, see source code of [answer_using_tools()]

#' @param parameter_fn A function that takes the [llm_provider-class] object
#'  which is being used with [send_prompt()] and returns a named list of parameters
#'  to be set in the [llm_provider-class] object via its `$set_parameters()` method.
#' This can be used to configure specific parameters of the [llm_provider-class]
#'  object when evaluating the prompt.
#' For example, [answer_as_json()] may set different parameters for different APIs
#'  related to JSON output.
#' This function is typically only used with advanced prompt wraps that require
#'  specific settings in the [llm_provider-class] object
#'
#' @param type The type of prompt wrap. Must be one of:
#' \itemize{
#' \item "unspecified": The default type, typically used for prompt wraps
#' which request a specific format of the LLM response, like [answer_as_integer()]
#'
#' \item "mode": For prompt wraps that change how the LLM should answer the prompt,
#' like [answer_by_chain_of_thought()] or [answer_by_react()]
#'
#' \item "tool": For prompt wraps that enable the LLM to use tools, like [answer_using_tools()]
#' or [answer_using_r()] when 'output_as_tool' = TRUE
#'
#' \item "break": For prompt wraps that may break the extraction and validation loop,
#'  like [quit_if()]. These are applied before type "unspecified" as they may
#'  instruct the LLM to not answer the prompt in the manner specified by those
#'  prompt wraps
#'
#' \item "check": For prompt wraps that apply a last check to the final answer,
#'  after all other prompt wraps have been evaluated.
#' These prompt wraps may only contain a validation function, and are applied
#'  after all other prompt wraps have been evaluated. These prompt wraps are
#'  even applied after an earlier prompt wrap has broken the extraction and validation loop
#'  with [llm_break()]
#' }
#' Types are used to determine the order in which prompt wraps are applied.
#' When constructing the prompt text, prompt wraps are applied to the base prompt
#'  in the following order: 'check', 'unspecified', 'break', 'mode', 'tool'.
#' When evaluating the LLM response and applying extraction and validation functions,
#'  prompt wraps are applied in the reverse order: 'tool', 'mode', 'break',
#'  'unspecified', 'check'.
#' Order among the same type is preserved in the order they were added to the prompt.

#' @param name An optional name for the prompt wrap.
#'  This can be used to identify the prompt wrap in the [tidyprompt-class] object
#'
#' @return A [tidyprompt-class] object with the [prompt_wrap()] appended to it
#'
#' @export
#'
#' @example inst/examples/prompt_wrap.R
#' @example R/answer_as_integer.R
#'
#' @family prompt_wrap
#' @family pre_built_prompt_wraps
#'
#' @seealso [tidyprompt-class] [send_prompt()]
prompt_wrap <- function(
    prompt,
    modify_fn = NULL,
    extraction_fn = NULL,
    validation_fn = NULL,
    handler_fn = NULL,
    parameter_fn = NULL,
    type = c("unspecified", "mode", "tool", "break", "check"),
    name = NULL
) {
  UseMethod("prompt_wrap")
}



#' [prompt_wrap()] method for when a [tidyprompt-class] object is supplied
#'
#' Calls the internal function to append the prompt wrap.
#'
#' @param prompt A single string or a [tidyprompt-class] object
#' @param ... Additional arguments
#'
#' @return A [tidyprompt-class] object with the [prompt_wrap()] appended to it
#'
#' @exportS3Method prompt_wrap tidyprompt
#'
#' @noRd
#' @keywords internal
prompt_wrap.tidyprompt <- function(prompt, ...) {
  prompt_wrap_internal(prompt, ...)
}



#' Default method for [prompt_wrap()]
#'
#' Attempts to create a [tidyprompt-class] object from whatever is passed as 'prompt';
#' then calls the internal function to append the [prompt_wrap()].
#'
#' @param prompt A single string or a [tidyprompt-class] object
#' @param ... Additional arguments
#'
#' @return A [tidyprompt-class] object with the [prompt_wrap()] appended to it
#'
#' @exportS3Method prompt_wrap default
#'
#' @noRd
#' @keywords internal
prompt_wrap.default <- function(prompt, ...) {
  prompt <- tidyprompt(prompt)
  prompt_wrap_internal(prompt, ...)
}



#' Internal function to append a [prompt_wrap()] to a [tidyprompt-class] object
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
#' @return A [tidyprompt-class] object with the [prompt_wrap()] appended to it
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
    type = c("unspecified", "mode", "tool", "break", "check"),
    name = NULL
) {
  type <- match.arg(type)

  stopifnot(
    is.null(modify_fn) || is.function(modify_fn),
    is.null(validation_fn) || is.function(validation_fn),
    is.null(extraction_fn) || is.function(extraction_fn),
    is.null(handler_fn) || is.function(handler_fn),
    is.null(parameter_fn) || is.function(parameter_fn)
  )

  if (type == "check") {
    if (is.null(validation_fn)) {
      stop("When using type 'check', validation_fn is required")
    }
    if (!all(sapply(list(modify_fn, extraction_fn, handler_fn, parameter_fn), is.null))) {
      stop("When using type 'check', only validation_fn is allowed")
    }
  }


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

    # Define the required arguments in the desired order, without overwriting the first argument's name
    required_args <- c("llm_provider", "http_list")

    # Combine the original arguments with the missing required arguments
    combined_args <- c(
      original_args,
      structure(rep(list(quote(expr = NULL)), length(required_args)), names = required_args)
    )

    # Remove excess arguments beyond the first three
    final_args <- combined_args[1:3]

    # Ensure the non-first required arguments are named appropriately
    if (length(final_args) > 1) {
      names(final_args)[-1] <- required_args[1:(length(final_args) - 1)]
    }

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
