#' Make LLM answer as a list of key-value pairs
#'
#' This function is similar to `answer_as_list()` but instead of returning
#' a list of items, it instructs the LLM to return a list of key-value pairs.
#'
#' @param prompt A single string or a [tidyprompt()] object
#' @param key_name (optional) A name or placeholder describing the "key" part
#'   of each pair
#' @param value_name (optional) A name or placeholder describing the "value" part
#'   of each pair
#' @param pair_explanation (optional) Additional explanation of what a pair should
#'   be. It should be a single string. It will be appended after the list instruction.
#' @param n_unique_items (optional) Number of unique key-value pairs required in the list
#' @param list_mode (optional) Mode of the list: "bullet" or "comma".
#'   - "bullet" mode expects pairs like:
#'     ```
#'     -- key1: value1
#'     -- key2: value2
#'     ```
#'   - "comma" mode expects pairs like:
#'     ```
#'     1. key: value, 2. key: value, etc.
#'     ```
#'
#' @return A [tidyprompt()] with an added [prompt_wrap()] which
#'   will ensure that the LLM response is a list of key-value pairs.
#'
#' @export
#'
#' @example inst/examples/answer_as_key_value.R
answer_as_key_value <- function(
    prompt,
    key_name = "key",
    value_name = "value",
    pair_explanation = NULL,
    n_unique_items = NULL,
    list_mode = c("bullet", "comma")
) {
  prompt <- tidyprompt(prompt)
  stopifnot(
    is.character(key_name), length(key_name) == 1,
    is.character(value_name), length(value_name) == 1,
    is.null(pair_explanation) || (is.character(pair_explanation) & length(pair_explanation) == 1),
    is.null(n_unique_items) || (is.numeric(n_unique_items) & n_unique_items > 0)
  )
  list_mode <- match.arg(list_mode)

  # Construct the example instructions
  if (list_mode == "bullet") {
    list_instruction <- glue::glue(
      "Respond with a list of key-value pairs, like so:\n",
      "  -- <<{key_name} 1>>: <<{value_name} 1>>", .trim = FALSE
    )

    if (is.null(n_unique_items) || n_unique_items > 1) {
      list_instruction <- glue::glue(
        "{list_instruction}\n",
        "  -- <<{key_name} 2>>: <<{value_name} 2>>", .trim = FALSE
      )
    }

    if (is.null(n_unique_items) || n_unique_items > 2) {
      list_instruction <- glue::glue(
        "{list_instruction}\n",
        "  etc.", .trim = FALSE
      )
    }

  } else if (list_mode == "comma") {
    list_instruction <- glue::glue(
      "Respond with a list of key-value pairs, like so:\n",
      "  1. <<{key_name}>>: <<{value_name}>>, 2. <<{key_name}>>: <<{value_name}>>, etc.",
      .trim = FALSE
    )
  }

  if (!is.null(n_unique_items)) {
    pair_or_pairs <- ifelse(n_unique_items == 1, "pair", "pairs")
    list_instruction <- glue::glue(
      "{list_instruction}\n",
      "The list should contain {n_unique_items} unique {pair_or_pairs}.",
      .trim = FALSE
    )
  }

  if (!is.null(pair_explanation)) {
    list_instruction <- glue::glue(
      "{list_instruction}\n\n",
      "{pair_explanation}", .trim = FALSE
    )
  }

  modify_fn <- function(original_prompt_text) {
    glue::glue(
      "{original_prompt_text}\n\n",
      "{list_instruction}", .trim = FALSE
    )
  }

  extraction_fn <- function(response) {
    if (!is.character(response)) {
      stop(paste0(
        "Response to extract must be a string. ",
        "Ensure that your LLM response is suitable for key-value pair extraction."
      ))
    }

    # Extract pairs depending on list_mode
    if (list_mode == "bullet") {
      # Pattern to capture lines starting with `-- ` followed by `key: value`
      pairs <- stringr::str_extract_all(response, "--\\s*([^:]+):\\s*([^\\n]+)")[[1]]
      if (length(pairs) == 0) {
        return(llm_feedback(glue::glue(
          "Could not parse any key-value pairs from your response.\n\n",
          "{list_instruction}"
        )))
      }

      # Split each match into key and value
      kv_pattern <- "^--\\s*([^:]+):\\s*(.*)$"
      keys <- stringr::str_replace(pairs, kv_pattern, "\\1") |> stringr::str_trim()
      vals <- stringr::str_replace(pairs, kv_pattern, "\\2") |> stringr::str_trim()

    } else if (list_mode == "comma") {
      # For comma mode, we need to handle cases where there might be commas or periods
      # Adjusted regex pattern to allow commas and periods at the end of the line
      matches <- stringr::str_match_all(response, "\\d+\\.\\s*([^:]+):\\s*([^,\\n.]+)")[[1]]
      if (nrow(matches) == 0) {
        return(llm_feedback(glue::glue(
          "Could not parse any key-value pairs from your response.\n\n",
          "{list_instruction}"
        )))
      }

      # Extract keys and values
      keys <- stringr::str_trim(matches[,2])
      vals <- stringr::str_trim(matches[,3])
    }

    # Create a data frame for the key-value pairs
    df <- data.frame(key = keys, value = vals, stringsAsFactors = FALSE)
    # Ensure uniqueness by key
    df <- df[!duplicated(df$key), ]

    # Check if the number of unique items matches the expected count
    if (!is.null(n_unique_items) && nrow(df) != n_unique_items) {
      return(llm_feedback(glue::glue(
        "The number of unique key-value pairs should be {n_unique_items}.\n\n",
        "{list_instruction}"
      )))
    }

    # Return as a named list
    result_list <- as.list(df$value)
    names(result_list) <- df$key
    return(result_list)
  }

  prompt_wrap(
    prompt, modify_fn, extraction_fn, name = "answer_as_key_value_list"
  )
}
