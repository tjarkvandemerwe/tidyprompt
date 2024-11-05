#' Make LLM answer as a list of items
#'
#' @param prompt A character string or a tidyprompt object
#' @param item_name (optional) Name of the items in the list
#' @param n_unique_items (optional) Number of unique items required in the list
#'
#' @return A tidyprompt with an added prompt wrapper object which
#' will ensure that the LLM response is a list of items.
#' @export
answer_as_list <- function(
    prompt,
    item_name = "item",
    n_unique_items = NULL
) {
  prompt <- tidyprompt(prompt)

  list_instruction <- glue::glue(
    "Respond with a list, like so:\n",
    "-- <<{item_name} 1>>\n",
  )
  if (!is.null(n_unique_items)) {
    if (!is.numeric(n_unique_items) || n_unique_items < 1) {
      stop("n_unique_items must be a positive integer")
    }
    if (n_unique_items != floor(n_unique_items)) {
      stop("n_unique_items must be a whole number")
    }

    if (n_unique_items > 1) { list_instruction <- glue::glue(
        "{list_instruction}\n",
        "-- <<{item_name} 2>>\n",
    )}
    if (n_unique_items > 2) { list_instruction <- glue::glue(
        "{list_instruction}\n",
        "etc.",
    )}

    list_instruction <- glue::glue(
      "{list_instruction}\n",
      "The list should contain {n_unique_items} unique items."
    )
  }

  modify_fn <- function(original_prompt_text) {
    glue::glue(
      "{original_prompt_text}\n\n",
      "{list_instruction}"
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
      return(create_llm_feedback(glue::glue(
        "Could not parse any listed items from your response.",
        "{list_instruction}"
      )))
    }

    # Make unique
    items <- unique(items)

    # Remove '--' prefix and trim whitespace
    items <- stringr::str_trim(stringr::str_remove_all(items, "--\\s*"))

    if (!is.null(n_unique_items) && length(items) != n_unique_items) {
      return(create_llm_feedback(glue::glue(
        "The number of unique items in your list should be {n_unique_items}.",
        "{list_instruction}"
      )))
    }

    # Return the extracted items
    return(items)
  }

  prompt_wrap(prompt, modify_fn, extraction_fn)
}
