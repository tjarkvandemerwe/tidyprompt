#### 1 Prompt wrap object & functions ####

#' Create a prompt wrap object
#'
#' @param type
#' Type of the prompt; can be "unspecified", "mode", or "tool".
#'  This is used to determine the order the prompt wrappers when constructing the final prompt text.
#'  Mode prompts and toolset prompts are placed at the bottom of the prompt list (toolset after mode).
#' @param modify_fn
#' Function that modifies the prompt text; takes two arguments: original_prompt_text and modify_fn_args.
#'  This function will be applied to the previous prompt text in the prompt list.
#' @param modify_fn_args
#' List of arguments to be passed to the modify_fn.
#' @param extraction_functions
#' List of functions that extract content from the response to the prompt.
#'  Should return the extracted object on successful extraction, or a 'llm_feedback' object upon failure.
#' @param validation_functions
#' List of functions that validate the (extracted) response to the prompt.
#'  Should return TRUE on successful validation, or a 'llm_feedback' object upon failure.
#' @param max_retries
#' Maximum number of retries for this wrap's validation and extraction functions.
#'
#' @return
#' A prompt wrap object
#'
#' @export
create_prompt_wrap <- function(
    type = c("unspecified", "mode", "tool"),
    modify_fn = NULL,
    validation_functions = list(),
    extraction_functions = list(),
    max_retries = 10
) {
  # match.arg for type
  type <- match.arg(type)

  # Verify modify_fn
  if (
    is.null(modify_fn)
    || !is.function(modify_fn)
    || length(formals(modify_fn)) != 2
    || !identical(names(formals(modify_fn)), c("original_prompt_text", "modify_fn_args"))
  )
    stop(paste0(
      "Modify_fn should be provided; it should be a function that takes two arguments: ",
      "original_prompt_text and modify_fn_args"
    ))

  # Create a list to hold the prompt details
  prompt_wrap <- list(
    type = type,
    modify_fn = modify_fn,
    validation_functions = validation_functions,
    extraction_functions = extraction_functions,
    max_retries = max_retries
  )

  class(prompt_wrap) <- "prompt_wrap"
  return(prompt_wrap)
}
