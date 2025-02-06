#' Convert a named or unnamed list/vector to a string representation
#'
#' Converts a named or unnamed list/vector to a string format, intended for
#' sending it to an LLM (or for display or logging).
#'
#' @param obj A list or vector (named or unnamed) to be converted to a string.
#' @param how In what way the object should be converted to a string;
#' either "inline" or "expanded". "inline" presents all key-value pairs
#' or values as a single line. "expanded" presents each key-value pair or value
#' on a separate line.
#'
#' @return A single string representing the list/vector.
#'
#' @export
#'
#' @example inst/examples/vector_list_to_string.R
#'
#' @family text_helpers
vector_list_to_string <- function(obj, how = c("inline", "expanded")) {
  how <- match.arg(how)

  # Check if input is a list or vector
  if (!is.list(obj) && !is.vector(obj)) {
    stop("Input must be a list or vector")
  }

  # Return empty string if object is empty
  if (length(obj) == 0) return("")

  # Handle named and unnamed cases
  if (!is.null(names(obj)) && any(names(obj) != "")) {
    # Named case: Generate output based on the `how` parameter
    if (how == "inline") {
      result_string <- paste(
        paste(names(obj), obj, sep = ": "),
        collapse = ", "
      )
    } else if (how == "expanded") {
      result_string <- paste(
        paste(names(obj), obj, sep = ": "),
        collapse = "\n"
      )
    }
  } else {
    # Unnamed case: Handle values only
    if (how == "inline") {
      result_string <- paste(obj, collapse = ", ")
    } else if (how == "expanded") {
      result_string <- paste(obj, collapse = "\n")
    }
  }

  return(result_string)
}
