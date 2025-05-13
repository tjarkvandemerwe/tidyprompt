#' Make LLM answer as a category
#'
#' @param prompt A single string or a [tidyprompt()] object
#' @param categories Possible categories to choose from (character vector)
#'
#' @return A [tidyprompt()] with an added [prompt_wrap()] which
#' will ensure that the LLM response is the most fitting category of a text as
#' a character vector of length one.
#' @export
answer_as_category <- function(
  prompt,
  categories
) {
  stopifnot(
    is.character(prompt),
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
    "Respond with the number of the category that best describes the text.",
    "\n",
    "(Use no other words or characters.)"
  )

  modify_fn <- function(original_prompt_text, numbered_categories) {
    paste0(
      "You need to categorize a text.\n\n",
      "Text:\n  '",
      original_prompt_text,
      "'\n\n",
      "Possible categories:\n  ",
      numbered_categories,
      "\n\n",
      instruction
    )
  }

  extraction_fn <- function(x) {
    normalized <- trimws(tolower(x))
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
#'
#' @param prompt A single string or a [tidyprompt()] object
#' @param categories Possible categories to choose from (character vector)
#'
#' @return A [tidyprompt()] with an added [prompt_wrap()] which
#' will ensure that the LLM response is a vector of fitting categories of a text.
#' @export
answer_as_multi_category <- function(
  prompt = "this product is red",
  categories = c(
    "positive review",
    "negative review",
    "mentions color",
    "does not mention color"
  )
) {
  stopifnot(
    is.character(text),
    is.character(categories),
    length(text) == 1,
    length(categories) > 0,
    !any(duplicated(categories))
  )

  numbered_categories <- paste0(
    seq_along(categories),
    ". ",
    categories,
    collapse = "\n  "
  )

  instruction <- "You need to categorize a text.\n\n"

  instruction <- paste0(
    instruction,
    "Text:\n  '",
    text,
    "'\n\n",
    "Possible categories:\n  ",
    numbered_categories,
    "\n\n",
    "Respond with the numbers of all categories that apply to this text, separated by commas.",
    "\n",
    "(Use only numbers separated by commas, no extra words or characters.)"
  )

  prompt <- instruction |>
    tidyprompt::prompt_wrap(
      extraction_fn = function(x) {
        normalized <- trimws(tolower(x))
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
    )

  return(prompt)
}
