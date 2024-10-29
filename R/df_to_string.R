#' Convert a dataframe to a string representation
#'
#' Converts a data frame to a string format, for feeding to a LLM
#' (or for display or logging).
#'
#' @param df A data frame to be converted to a string.
#' @param how
#' @return A single string representing the data frame.
#' @examples
#' df <- data.frame(Name = c("Alice", "Bob"), Age = c(25, 30))
#' df_to_string(df, wide = FALSE)
#' df_to_string(df, wide = TRUE)
df_to_string <- function(df, how = c("wide", "long")) {
  how <- match.arg(how)

  # Check if input is a data frame
  if (!is.data.frame(df)) {
    stop("Input must be a data frame")
  }

  # Return empty string if data frame has no rows
  if (nrow(df) == 0) return("")

  # Get column names
  column_names <- colnames(df)

  # Generate the output based on the `wide` parameter
  if (how == "wide") {
    # Concatenate column names as the header
    header <- paste(column_names, collapse = ", ")

    # Convert each row into a single line string without column names
    rows_as_strings <- apply(df, 1, function(row) {
      paste(row, collapse = ", ")
    })

    # Combine header and rows into a single output string
    result_string <- paste(c(header, rows_as_strings), collapse = "\n")

  } else if (how == "long") {
    # Convert each row with column names for each value
    rows_as_strings <- apply(df, 1, function(row) {
      paste(paste(column_names, row, sep = ": "), collapse = "\n")
    })

    # Combine rows into a single output string
    result_string <- paste(rows_as_strings, collapse = "\n\n\n")
    result_string <- paste0(result_string, "\n\n\n")
  }

  return(result_string)
}
