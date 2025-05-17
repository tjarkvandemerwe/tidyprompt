#' Make LLM answer as a category
#' For multiple categories, see [answer_as_multi_category()]
#'
#' @param prompt A single string or a [tidyprompt()] object
#' @param categories Possible categories to choose from (character vector)
#'
#' @return A [tidyprompt()] with an added [prompt_wrap()] which
#' will ensure that the LLM response is the most fitting category of a text as
#' a character vector of length one.
#'
#' @export
#'
#' @example inst/examples/answer_as_category.R
#'
#' @family pre_built_prompt_wraps
#' @family answer_as_prompt_wraps
answer_as_category <- function(
  prompt,
  categories
) {
  prompt <- tidyprompt(prompt)

  stopifnot(
    is.character(categories),
    length(categories) > 0,
    !any(duplicated(categories))
  )

  numbered_categories <- paste0(
    seq_along(categories),
    ". ",
    categories,
    collapse = "\n  "
  )

  instruction <- paste0(
    "Possible categories:\n  ",
    numbered_categories,
    "\n\n",
    "Respond with the number of the category that best describes the text.",
    "\n",
    "(Use no other words or characters.)"
  )

  modify_fn <- function(original_prompt_text) {
    paste0(
      "You need to categorize a text.\n\n",
      "Text:\n  '",
      original_prompt_text,
      "'\n\n",
      instruction
    )
  }

  extraction_fn <- function(response) {
    normalized <- trimws(tolower(response))
    if (normalized %in% as.character(seq_along(categories))) {
      return(categories[[as.integer(normalized)]])
    }
    match <- which(tolower(categories) == normalized)
    if (length(match) == 1) {
      return(categories[[match]])
    }
    return(tidyprompt::llm_feedback(instruction))
  }

  prompt_wrap(
    prompt,
    modify_fn,
    extraction_fn,
    name = "answer_as_category"
  )
}


#' Build prompt for categorizing a text into multiple categories
#' For single category, see [answer_as_category()]
#'
#' @param prompt A single string or a [tidyprompt()] object
#' @param categories Possible categories to choose from (character vector)
#'
#' @return A [tidyprompt()] with an added [prompt_wrap()] which
#' will ensure that the LLM response is a vector of fitting categories of a text.
#'
#' @export
#'
#' @example inst/examples/answer_as_multi_category.R
#'
#' @family pre_built_prompt_wraps
#' @family answer_as_prompt_wraps
answer_as_multi_category <- function(
  prompt,
  categories
) {
  sprompt <- tidyprompt(prompt)

  stopifnot(
    is.character(categories),
    length(categories) > 0,
    !any(duplicated(categories))
  )

  numbered_categories <- paste0(
    seq_along(categories),
    ". ",
    categories,
    collapse = "\n  "
  )

  instruction <- paste0(
    "Possible categories:\n  ",
    numbered_categories,
    "\n\n",
    "Respond with the numbers of all categories that apply to this text, separated by commas.",
    "\n",
    "(Use no other words or characters.)"
  )

  modify_fn <- function(original_prompt_text) {
    paste0(
      "You need to categorize a text.\n\n",
      "Text:\n  '",
      original_prompt_text,
      "'\n\n",
      instruction
    )
  }

  extraction_fn <- function(response) {
    normalized <- trimws(tolower(response))
    numbers <- unlist(strsplit(normalized, ",\\s*"))
    valid_numbers <- numbers[
      numbers %in% as.character(seq_along(categories))
    ]
    if (length(valid_numbers) == 0) {
      return(tidyprompt::llm_feedback(
        "You must select at least one valid category number."
      ))
    }
    categories_selected <- categories[as.integer(valid_numbers)]
    return(
      jsonlite::toJSON(categories_selected, auto_unbox = FALSE)
    )
  }

  prompt_wrap(
    prompt,
    modify_fn,
    extraction_fn,
    name = "answer_as_multi_category"
  )
}
