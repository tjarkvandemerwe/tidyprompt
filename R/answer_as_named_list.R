#' Make LLM answer as a named list
#'
#' Get a named list from LLM response with optional item instructions and validations.
#'
#' @param prompt A single string or a [tidyprompt()] object
#' @param item_names A character vector specifying the expected item names
#' @param item_instructions An optional named list of additional instructions for each item
#' @param item_validations An optional named list of validation functions for each item.
#' Like validation functions for a [prompt_wrap()], these functions should return
#' [llm_feedback()] if the validation fails. If the validation
#' is successful, the function should return TRUE
#'
#' @return A [tidyprompt()] with an added [prompt_wrap()] that ensures
#' the LLM response is a named list with the specified item names, optional
#' instructions, and validations.
#'
#' @export
#'
#' @example inst/examples/answer_as_named_list.R
#'
#' @seealso [answer_as_list()] [llm_feedback()]
#'
#' @family pre_built_prompt_wraps
#' @family answer_as_prompt_wraps
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
    if (
      !is.list(item_instructions) ||
        !all(names(item_instructions) %in% item_names)
    ) {
      stop(
        "`item_instructions` must be a named list with names matching `item_names`."
      )
    }
  }

  if (!is.null(item_validations)) {
    if (
      !is.list(item_validations) ||
        !all(names(item_validations) %in% item_names)
    ) {
      stop(
        "`item_validations` must be a named list with names matching `item_names`."
      )
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
          glue::glue("  -- {x}: <<value>>{instruction}")
        }
      ),
      collapse = "\n"
    ),
    "\n\nItem names in the list must correspond to:\n",
    "  {paste(item_names, collapse = ', ')}",
    .trim = FALSE
  )

  modify_fn <- function(original_prompt_text) {
    glue::glue(
      "{original_prompt_text}\n\n",
      "{list_instruction}"
    )
  }

  extraction_fn <- function(response) {
    if (!is.character(response)) {
      stop(
        paste0(
          "Response to extract a list from must be a string.",
          " Ensure no prompt wraps make the response unsuitable for list extraction."
        )
      )
    }

    # Use regex to extract named key-value pairs
    named_items <- stringr::str_match_all(response, "--\\s*([^:]+):\\s*(.+)")[[
      1
    ]]

    if (nrow(named_items) == 0) {
      return(
        llm_feedback(
          glue::glue(
            "Could not parse any named items from your response.\n\n",
            "{list_instruction}"
          )
        )
      )
    }

    # Extract names and values
    names <- stringr::str_trim(named_items[, 2])
    values <- stringr::str_trim(named_items[, 3])

    # Create named list
    named_list <- values
    names(named_list) <- names

    # Validate all expected names are present
    missing_names <- setdiff(item_names, names(named_list))
    if (length(missing_names) > 0) {
      return(
        llm_feedback(
          glue::glue(
            "The response is missing the following expected names: {paste(missing_names, collapse = ', ')}.\n",
            "{list_instruction}"
          )
        )
      )
    }

    # Return the named list
    named_list |> as.list()
  }

  validation_fn <- function(named_list) {
    # Apply validation functions if provided
    if (!is.null(item_validations)) {
      validation_results <- mapply(
        function(x, validation_fn) {
          validation_result <- validation_fn(x)
          if (isTRUE(validation_result)) {
            NULL # Validation passed
          } else {
            validation_result # Return validation error message or object
          }
        },
        named_list[names(item_validations)],
        item_validations,
        SIMPLIFY = FALSE
      )

      # Check if any validation failed
      failed_validations <- validation_results[
        !sapply(validation_results, is.null)
      ]
      if (length(failed_validations) > 0) {
        failed_messages <- sapply(
          names(failed_validations),
          function(name) paste0("-- ", name, ": ", failed_validations[[name]]),
          USE.NAMES = FALSE
        )
        return(
          llm_feedback(
            glue::glue(
              "Validation failed for the following items:\n",
              paste(failed_messages, collapse = "\n"),
              "\n{list_instruction}"
            )
          )
        )
      }
    }

    return(TRUE)
  }

  prompt_wrap(
    prompt,
    modify_fn,
    extraction_fn,
    validation_fn,
    name = "answer_as_named_list"
  )
}
