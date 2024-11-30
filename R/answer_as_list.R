#' Make LLM answer as a list of items
#'
#' @param prompt A single string or a [tidyprompt()] object
#' @param item_name (optional) Name of the items in the list
#' @param item_explanation (optional) Additional explanation of what an item
#' should be. Item explanation should be a single string. It will be
#' appended after the list instruction
#' @param n_unique_items (optional) Number of unique items required in the list
#' @param list_mode (optional) Mode of the list. Either "bullet" or "comma".
#' "bullet mode expects items to be listed with "--" before each item, with a
#'  new line for each item (e.g., "-- item1\\n-- item2\\n-- item3").
#'  "comma" mode expects items to be listed with a number and a period before
#'  (e.g., "1. item1, 2. item2, 3. item3"). "comma" mode may be easier for
#'  smaller LLMs to use
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
#' @family pre_built_prompt_wraps
#' @family answer_as_prompt_wraps
answer_as_list <- function(
    prompt,
    item_name = "item",
    item_explanation = NULL,
    n_unique_items = NULL,
    list_mode = c("bullet", "comma")
) {
  prompt <- tidyprompt(prompt)
  stopifnot(
    is.character(item_name), length(item_name) == 1,
    is.null(item_explanation) || (is.character(item_explanation) & length(item_explanation) == 1),
    is.null(n_unique_items) || (is.numeric(n_unique_items) & n_unique_items > 0)
  )
  list_mode <- match.arg(list_mode)

  # Adjust list instruction based on list_mode
  if (list_mode == "bullet") {
    list_instruction <- glue::glue(
      "Respond with a list, like so:\n",
      "  -- <<{item_name} 1>>",
      .trim = FALSE
    )

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
  } else if (list_mode == "comma") {
    list_instruction <- glue::glue(
      "Respond with a list, like so:\n",
      "  1. <<{item_name}>>",
      .trim = FALSE
    )

    if (is.null(n_unique_items) || n_unique_items > 1) {
      list_instruction <- glue::glue(
        "{list_instruction}, 2. <<{item_name}>>",
        .trim = FALSE
      )
    }

    if (is.null(n_unique_items) || n_unique_items > 2) {
      list_instruction <- glue::glue(
        "{list_instruction}, etc.",
        .trim = FALSE
      )
    }
  }

  if (!is.null(n_unique_items)) {
    item_or_items <- ifelse(n_unique_items == 1, "item", "items")

    list_instruction <- glue::glue(
      "{list_instruction}\n",
      "The list should contain {n_unique_items} unique {item_or_items}.",
      .trim = FALSE
    )
  }

  if (!is.null(item_explanation)) {
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
        "Response to extract a list from must be a string.",
        " Make sure that you do not apply any prompt wraps which make the",
        " response unsuitable for list extraction."
      ))
    }

    if (list_mode == "bullet") {
      # Existing bullet mode extraction
      items <- stringr::str_extract_all(response, "--\\s*([^\\-\\n]+)")[[1]]
      if (length(items) == 0) {
        return(llm_feedback(glue::glue(
          "Could not parse any listed items from your response.",
          "{list_instruction}",
          .trim = FALSE
        )))
      }
      items <- stringr::str_trim(stringr::str_remove_all(items, "--\\s*"))
    } else if (list_mode == "comma") {
      # Improved comma mode extraction
      items <- stringr::str_match_all(response, "\\d+\\.\\s*([^,\\n]+)")[[1]][,2]

      if (length(items) == 0) {
        return(llm_feedback(glue::glue(
          "Could not parse any listed items from your response.",
          "{list_instruction}",
          .trim = FALSE
        )))
      }

      # Trim whitespace
      items <- stringr::str_trim(items)

      # Remove any leading numbering (e.g., "1. ", "2. ")
      items <- stringr::str_replace(items, "^\\d+\\.\\s*", "")

      # Remove empty strings
      items <- items[items != ""]

      if (length(items) == 0) {
        return(llm_feedback(glue::glue(
          "Could not parse any valid items from your response.",
          "{list_instruction}",
          .trim = FALSE
        )))
      }
    }

    # Make items unique
    items <- unique(items)

    if (!is.null(n_unique_items) && length(items) != n_unique_items) {
      return(llm_feedback(glue::glue(
        "The number of unique items in your list should be {n_unique_items}.",
        "{list_instruction}",
        .trim = FALSE
      )))
    }

    # Return the extracted items
    return(as.list(items))
  }

  prompt_wrap(prompt, modify_fn, extraction_fn)
}

