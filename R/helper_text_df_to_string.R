#' Convert a dataframe to a string representation
#'
#' Converts a data frame to a string format, intended for sending it to a LLM
#' (or for display or logging).
#'
#' @param df A `data.frame` object to be converted to a string
#' @param how In what way the df should be converted to a string;
#' either "wide" or "long". "wide" presents column names on the first row,
#' followed by the row values on each new row. "long" presents the values
#' of each row together with the column names, repeating for every row
#' after two lines of whitespace
#'
#' @return A single string representing the df
#'
#' @export
#' @example inst/examples/df_to_string.R
#'
#' @family text_helpers
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
