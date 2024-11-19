#' Make LLM answer as a list of items
#'
#' @param prompt A single string or a [tidyprompt()] object
#' @param item_name (optional) Name of the items in the list
#' @param item_explanation (optional) Additional explanation of what an item
#' should be. Item explanation should be a single string. It will be
#' appended after the list instruction
#' @param n_unique_items (optional) Number of unique items required in the list
#'
#' @return A [tidyprompt()] with an added [prompt_wrap()] which
#' will ensure that the LLM response is a list of items
#'
#' @export
#'
#' @example inst/examples/answer_as_list.R
#'
#' @seealso [answer_as_named_list()]
#'
#' @family prompt_wrap
#' @family pre_built_prompt_wraps
#' @family answer_as_prompt_wraps
answer_as_list <- function(
    prompt,
    item_name = "item",
    item_explanation = NULL,
    n_unique_items = NULL
) {
  prompt <- tidyprompt(prompt)

  list_instruction <- glue::glue(
    "Respond with a list, like so:\n",
    "  -- <<{item_name} 1>>",
    .trim = FALSE
  )

  if (!is.null(n_unique_items)) {
    if (!is.numeric(n_unique_items) || n_unique_items < 1) {
      stop("n_unique_items must be a positive integer")
    }
    if (n_unique_items != floor(n_unique_items)) {
      stop("n_unique_items must be a whole number")
    }
  }

  if (is.null(n_unique_items) || n_unique_items > 1) {
    list_instruction <- glue::glue(
      "{list_instruction}\n",
      "  -- <<{item_name} 2>>",
      .trim = FALSE
    )
  }

  if (is.null(n_unique_items) || n_unique_items > 2) {
    list_instruction <- glue::glue(
      "{list_instruction}\n",
      "  etc.",
      .trim = FALSE
    )
  }

  if (!is.null(n_unique_items)) {
    item_or_items <- "items"
    if (n_unique_items == 1) {
      item_or_items <- "item"
    }

    list_instruction <- glue::glue(
      "{list_instruction}\n",
      "The list should contain {n_unique_items} unique {item_or_items}.",
      .trim = FALSE
    )
  }

  if (!is.null(item_explanation)) {
    if (!is.character(item_explanation)) {
      stop("item_explanation must be a string")
    }
    if (length(item_explanation) > 1) {
      stop("item_explanation must be a single string")
    }

    list_instruction <- glue::glue(
      "{list_instruction}\n\n",
      "{item_explanation}",
      .trim = FALSE
    )
  }

  modify_fn <- function(original_prompt_text) {
    glue::glue(
      "{original_prompt_text}\n\n",
      "{list_instruction}",
      .trim = FALSE
    )
  }

  extraction_fn <- function(response) {
    if (!is.character(response)) {
      stop(paste0(
        "Response to extract a list from must be a string",
        " make sure that you do not apply any prompt wraps which make the",
        " response unsuitable for list extraction."
      ))
    }

    # Use str_extract_all to find all instances of '--' followed by multiple words
    items <- stringr::str_extract_all(response, "--\\s*([^\\-\\n]+)")[[1]]

    if (length(items) == 0) {
      return(llm_feedback(glue::glue(
        "Could not parse any listed items from your response.",
        "{list_instruction}",
        .trim = FALSE
      )))
    }

    # Make unique
    items <- unique(items)

    # Remove '--' prefix and trim whitespace
    items <- stringr::str_trim(stringr::str_remove_all(items, "--\\s*"))

    if (!is.null(n_unique_items) && length(items) != n_unique_items) {
      return(llm_feedback(glue::glue(
        "The number of unique items in your list should be {n_unique_items}.",
        "{list_instruction}",
        .trim = FALSE
      )))
    }

    # Return the extracted items
    return(items)
  }

  prompt_wrap(prompt, modify_fn, extraction_fn)
}
