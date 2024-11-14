#' Make LLM answer as a list of items
#'
#' @param prompt A character string or a tidyprompt object
#' @param item_name (optional) Name of the items in the list
#' @param n_unique_items (optional) Number of unique items required in the list
#'
#' @return A tidyprompt with an added prompt wrapper object which
#' will ensure that the LLM response is a list of items.
#'
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



#' Extract named list from LLM response with optional item instructions and validations
#'
#' @param prompt A character string or a tidyprompt object.
#' @param item_names A character vector specifying the expected item names.
#' @param item_instructions An optional named list of additional instructions for each item.
#' @param item_validations An optional named list of validation functions for each item.
#'
#' @return A tidyprompt with an added prompt wrapper object that ensures
#' the LLM response is a named list with the specified item names, optional instructions, and validations.
#'
#' @export
answer_as_named_list <- function(
    prompt,
    item_names,
    item_instructions = NULL,
    item_validations = NULL
) {
  prompt <- tidyprompt(prompt)

  if (!is.character(item_names) || length(item_names) == 0) {
    stop("`item_names` must be a non-empty character vector.")
  }

  if (!is.null(item_instructions)) {
    if (!is.list(item_instructions) || !all(names(item_instructions) %in% item_names)) {
      stop("`item_instructions` must be a named list with names matching `item_names`.")
    }
  }

  if (!is.null(item_validations)) {
    if (!is.list(item_validations) || !all(names(item_validations) %in% item_names)) {
      stop("`item_validations` must be a named list with names matching `item_names`.")
    }
  }

  # Generate instructions for the expected format
  list_instruction <- glue::glue(
    "Respond with a named list like so:\n",
    paste(
      sapply(
        item_names,
        function(x) {
          instruction <- if (!is.null(item_instructions[[x]])) {
            glue::glue(" ({item_instructions[[x]]})")
          } else {
            ""
          }
          glue::glue("-- {x}: <<value>>{instruction}")
        }
      ),
      collapse = "\n"
    ),
    "\nEach name must correspond to: {paste(item_names, collapse = ', ')}"
  )

  modify_fn <- function(original_prompt_text) {
    glue::glue(
      "{original_prompt_text}\n\n",
      "{list_instruction}"
    )
  }

  extraction_fn <- function(response) {
    if (!is.character(response)) {
      stop(paste0(
        "Response to extract a list from must be a string.",
        " Ensure no prompt wraps make the response unsuitable for list extraction."
      ))
    }

    # Use regex to extract named key-value pairs
    named_items <- stringr::str_match_all(response, "--\\s*([^:]+):\\s*(.+)")[[1]]

    if (nrow(named_items) == 0) {
      return(create_llm_feedback(glue::glue(
        "Could not parse any named items from your response.",
        "{list_instruction}"
      )))
    }

    # Extract names and values
    names <- stringr::str_trim(named_items[, 2])
    values <- stringr::str_trim(named_items[, 3])

    # Create named list
    named_list <- setNames(values, names)

    # Validate all expected names are present
    missing_names <- setdiff(item_names, names(named_list))
    if (length(missing_names) > 0) {
      return(create_llm_feedback(glue::glue(
        "The response is missing the following expected names: {paste(missing_names, collapse = ', ')}.\n",
        "{list_instruction}"
      )))
    }

    # Return the named list
    named_list
  }

  validation_fn <- function(named_list) {
    # Apply validation functions if provided
    if (!is.null(item_validations)) {
      validation_results <- mapply(
        function(x, validation_fn) {
          validation_result <- validation_fn(x)
          if (isTRUE(validation_result)) {
            NULL  # Validation passed
          } else {
            validation_result  # Return validation error message or object
          }
        },
        named_list[names(item_validations)],
        item_validations,
        SIMPLIFY = FALSE
      )

      # Check if any validation failed
      failed_validations <- validation_results[!sapply(validation_results, is.null)]
      if (length(failed_validations) > 0) {
        failed_messages <- sapply(
          names(failed_validations),
          function(name) paste0("-- ", name, ": ", failed_validations[[name]]),
          USE.NAMES = FALSE
        )
        return(create_llm_feedback(glue::glue(
          "Validation failed for the following items:\n",
          paste(failed_messages, collapse = "\n"),
          "\n{list_instruction}"
        )))
      }
    }
  }

  prompt_wrap(prompt, modify_fn, extraction_fn, validation_fn)
}



