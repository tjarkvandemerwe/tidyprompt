#' Make evaluation of a prompt stop if LLM gives a specific response
#'
#' This function is used to wrap a [tidyprompt()] object and ensure that the
#' evaluation will stop if the LLM says it cannot answer the prompt. This is
#' useful in scenarios where it is determined the LLM is unable to provide a
#' response to a prompt.
#'
#' @param prompt A single string or a [tidyprompt()] object
#' @param quit_detect_regex A regular expression to detect in the LLM's
#' response which will cause the evaluation to stop. The default
#' will detect the string "NO ANSWER" in the response
#' @param instruction A string to be added to the prompt to instruct the LLM
#' how to respond if it cannot answer the prompt. The default is
#' "If you think that you cannot provide a valid answer, you must type: 'NO ANSWER' (use no other characters)".
#' This parameter can be set to `NULL` if no instruction is needed in the prompt
#' @param success A logical indicating whether the [send_prompt()] loop break
#' should nonetheless be considered as a successful completion of the
#' extraction and validation process. If `FALSE`, the `object_to_return` must
#' will always be set to NULL and thus parameter 'response_result' must also
#' be set to 'null'; if `FALSE`, [send_prompt()] will also print a warning
#' about the unsuccessful evaluation. If `TRUE`, the `object_to_return` will be
#' returned as the response result of [send_prompt()] (and [send_prompt()]
#' will print no warning about unsuccessful evaluation); parameter 'response_result'
#' will then determine what is returned as the response result of [send_prompt()].
#' @param response_result A character string indicating what should be returned
#' when the quit_detect_regex is detected in the LLM's response. The default is
#' 'null', which will return NULL as the response result o f [send_prompt()].
#'  Under 'llm_response', the full LLM response will be returned as the
#' response result of [send_prompt()].
#'  Under 'regex_match', the part of the LLM response that matches the
#' quit_detect_regex will be returned as the response result of [send_prompt()]
#'
#' @return A [tidyprompt()] with an added [prompt_wrap()] which will ensure
#' that the evaluation will stop upon detection of the quit_detect_regex in the
#' LLM's response
#'
#' @export
#'
#' @example inst/examples/quit_if.R
#'
#' @family pre_built_prompt_wraps
#' @family miscellaneous_prompt_wraps
quit_if <- function(
  prompt,
  quit_detect_regex = "NO ANSWER",
  instruction = paste0(
    "If you think that you cannot provide a valid answer, you must type:\n",
    "'NO ANSWER' (use no other characters)"
  ),
  success = TRUE,
  response_result = c("null", "llm_response", "regex_match")
) {
  # Validate arguments
  if (!is.null(instruction)) {
    if (!is.character(instruction)) {
      stop("The 'instruction' argument must be of type character.")
    }
    if (length(instruction) != 1) {
      stop("The 'instruction' argument must be a single string.")
    }
  }
  if (!is.character(quit_detect_regex)) {
    stop("The 'quit_detect_regex' argument must be of type character.")
  }
  if (length(quit_detect_regex) != 1) {
    stop("The 'quit_detect_regex' argument must be a single string.")
  }
  if (!is.logical(success)) {
    stop("The 'success' argument must be of type logical.")
  }
  response_result <- match.arg(response_result)
  if (!success & response_result != "null") {
    stop("If 'success' is FALSE, 'response_result' must be of type 'null'")
  }

  modify_fn <- function(original_prompt_text) {
    if (is.null(instruction)) {
      return(original_prompt_text)
    }
    glue::glue("{original_prompt_text}\n\n{instruction}")
  }

  extraction_fn <- function(x) {
    if (grepl(quit_detect_regex, x)) {
      if (response_result == "null") {
        return(
          llm_break(
            object_to_return = NULL,
            success = success
          )
        )
      }
      if (response_result == "llm_response") {
        return(
          llm_break(
            object_to_return = x,
            success = success
          )
        )
      }
      if (response_result == "regex_match") {
        return(
          llm_break(
            object_to_return = regmatches(x, regexpr(quit_detect_regex, x))[[
              1
            ]],
            success = success
          )
        )
      }
    }
    return(x)
  }

  prompt_wrap(prompt, modify_fn, extraction_fn, type = "break")
}
