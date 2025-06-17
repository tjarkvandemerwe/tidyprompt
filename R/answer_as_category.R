#' Make LLM answer as a category
#' For multiple categories, see [answer_as_multi_category()]
#'
#' @param prompt A single string or a [tidyprompt()] object
#' @param categories A character vector of category names. Must not be empty
#'   and must not contain duplicates.
#' @param descriptions An optional character vector of descriptions,
#'   corresponding to each category. If provided, its length must match
#'   the length of `categories`. Defaults to `NULL`.
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
  categories,
  descriptions = NULL
) {
  prompt <- tidyprompt(prompt)

  numbered_categories_text <- .build_formatted_category_list(
    categories,
    descriptions
  )

  instruction <- paste0(
    numbered_categories_text,
    "\n\n",
    "Respond with the number of the category that best describes the text.",
    "\n",
    "(Use no other words or characters.)"
  )

  modify_fn <- function(original_prompt_text) {
    paste0(
      "You need to categorize the following text.\n\n",
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
#' @param categories A character vector of category names. Must not be empty
#'   and must not contain duplicates.
#' @param descriptions An optional character vector of descriptions,
#'   corresponding to each category. If provided, its length must match
#'   the length of `categories`. Defaults to `NULL`.
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
  categories,
  descriptions = NULL
) {
  prompt <- tidyprompt(prompt)

  numbered_categories_text <- .build_formatted_category_list(
    categories,
    descriptions
  )

  instruction <- paste0(
    numbered_categories_text,
    "\n\n",
    "Respond with the numbers of all categories that apply to this text, separated by commas.",
    "\n",
    "(Use no other words or characters.)"
  )

  modify_fn <- function(original_prompt_text) {
    paste0(
      "You need to categorize the following text.\n\n",
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

    return(categories_selected)
  }

  prompt_wrap(
    prompt,
    modify_fn,
    extraction_fn,
    name = "answer_as_multi_category"
  )
}

#' Build formatted categories text
#'
#' This helper function generates a text block listing categories,
#' optionally with their descriptions.
#' It also provides an appropriate introductory phrase.
#'
#' @param categories A character vector of category names. Must not be empty
#'   and must not contain duplicates.
#' @param descriptions An optional character vector of descriptions,
#'   corresponding to each category. If provided, its length must match
#'   the length of `categories`. Defaults to `NULL`.
#'
#' @return A string.
#'
#' @noRd
#'
#' @keywords internal
.build_formatted_category_list <- function(
  categories,
  descriptions = NULL
) {
  stopifnot(
    "'categories' must be a character vector." = is.character(categories),
    "'categories' must not be empty." = length(categories) > 0,
    "'categories' must not contain duplicates." = !any(duplicated(categories))
  )

  if (!is.null(descriptions)) {
    length_equal <- length(descriptions) == length(categories)
    stopifnot(
      "'descriptions' must be a character vector." = is.character(descriptions),
      "'descriptions' must have the same length as 'categories'." = length_equal
    )
  }

  if (is.null(descriptions)) {
    category_entries <- sprintf(
      "%d. %s",
      seq_along(categories),
      categories
    )
    instruction_intro <- "Possible categories:"
  } else {
    category_entries <- sprintf(
      "%d. %s: %s",
      seq_along(categories),
      categories,
      descriptions
    )
    instruction_intro <- "Possible categories and their descriptions:"
  }

  numbered_categories_text <- paste0(category_entries, collapse = "\n  ")

  formatted_category_list <- paste0(
    instruction_intro,
    "\n  ",
    numbered_categories_text
  )

  return(formatted_category_list)
}
