#' Make LLM answer match a specific regex
#'
#' @param prompt A single string or a [tidyprompt()] object
#' @param regex A character string specifying the regular expression the response must match
#' @param mode A character string specifying the mode of the regex match. Options are
#' "exact_match" (default) and "extract_all_matches".
#'  Under "full_match", the full LLM response must match the regex. If
#'  it does not, the LLM will be sent feedback to retry. The full LLM response
#'  will be returned if the regex is matched.
#'  Under "extract_matches", all matches of the regex in the LLM response will be returned
#'  (if present). If the regex is not matched at all, the LLM will be sent feedback to retry.
#'  If there is at least one match, the matches will be returned as a character vector
#'
#' @return A [tidyprompt()] with an added [prompt_wrap()] which will ensure
#' that the LLM response matches the specified regex
#'
#' @export
#'
#' @example inst/examples/answer_as_regex.R
#'
#' @family pre_built_prompt_wraps
#' @family answer_as_prompt_wraps
answer_as_regex <- function(
    prompt,
    regex,
    mode = c("full_match", "extract_matches")
) {
  mode <- match.arg(mode)

  # Default instruction for LLM if regex is provided
  instruction <- glue::glue(
    "You must answer with a response that matches this regex format:\n",
    "  {regex}",
    .trim = FALSE
  )
  if (mode == "full_match") {
    instruction <- glue::glue(
      "{instruction}\n",
      "  (use no other characters)",
      .trim = FALSE
    )
  }

  modify_fn <- function(original_prompt_text) {
    glue::glue("{original_prompt_text}\n\n{instruction}", .trim = FALSE)
  }

  extraction_fn <- function(x) {
    if (mode == "full_match") {
      if (!grepl(regex, x)) {
        return(llm_feedback(instruction))
      }
      return(x)
    }

    if (mode == "extract_matches") {
      # Extract all matches using gregexpr
      matches <- unlist(regmatches(x, gregexpr(regex, x)))
      # Remove empty matches
      matches <- matches[matches != ""]
      if (length(matches) == 0) {
        return(llm_feedback(instruction))
      }
      return(matches)
    }
  }

  prompt_wrap(prompt, modify_fn, extraction_fn, name = "answer_as_regex")
}
