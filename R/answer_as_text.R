#' Make LLM answer as a constrained text response
#'
#' @param prompt A single string or a [tidyprompt()] object
#' @param max_words (optional) Maximum number of words allowed in the response.
#' If specified, responses exceeding this limit will fail validation.
#' @param max_characters (optional) Maximum number of characters allowed in the response.
#' If specified, responses exceeding this limit will fail validation.
#' @param add_instruction_to_prompt (optional) Add instruction for replying
#' within the constraints to the prompt text. Set to FALSE for debugging if
#' extractions/validations are working as expected (without instruction the
#' answer should fail the validation function, initiating a retry).
#'
#' @return A [tidyprompt()] with an added [prompt_wrap()] which
#' will ensure that the LLM response conforms to the specified constraints.
#'
#' @export
#'
#' @example inst/examples/answer_as_text.R
#'
#' @family pre_built_prompt_wraps
#' @family answer_as_prompt_wraps
answer_as_text <- function(
    prompt,
    max_words = NULL,
    max_characters = NULL,
    add_instruction_to_prompt = TRUE
) {
  instruction <- "You must provide a text response."

  if (!is.null(max_words))
    instruction <- paste(instruction, glue::glue("The response must be at most {max_words} words."))

  if (!is.null(max_characters))
    instruction <- paste(instruction, glue::glue("The response must be at most {max_characters} characters."))

  modify_fn <- function(original_prompt_text) {
    if (!add_instruction_to_prompt) {
      return(original_prompt_text)
    }

    glue::glue("{original_prompt_text}\n\n{instruction}")
  }

  extraction_fn <- function(x) {
    trimmed <- trimws(x)
    word_count <- strsplit(trimmed, "\\s+")[[1]] |> length()
    char_count <- nchar(trimmed)

    if (!is.null(max_words) && word_count > max_words) {
      return(llm_feedback(glue::glue("Response exceeds the maximum word limit of {max_words}.")))
    }

    if (!is.null(max_characters) && char_count > max_characters) {
      return(llm_feedback(glue::glue("Response exceeds the maximum character limit of {max_characters}.")))
    }

    return(trimmed)
  }

  prompt_wrap(
    prompt, modify_fn, extraction_fn,
    name = "answer_as_text"
  )
}
