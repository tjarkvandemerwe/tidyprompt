#' Skim a dataframe and include labels and levels
#'
#' This function takes a dataframe and returns a skim summary with variable names,
#' labels, and levels for categorical variables. It is a wrapper around the skim()
#' function from the 'skimr' package.
#'
#' @param data A dataframe to be skimmed
#'
#' @return A dataframe with variable names, labels, levels, and skim summary
#'
#' @export
#'
#' @family text_helpers
skim_with_labels_and_levels <- function(data) {
  # Extract labels and levels from data
  labels <- sapply(data, function(x) ifelse(is.null(attr(x, "label")), NA, attr(x, "label")))
  levels_list <- sapply(data, function(x) if (is.factor(x)) levels(x) else NA)

  # Create a data frame with variable names, labels, and levels
  labels_df <- data.frame(
    variable = names(labels),
    description = labels,
    levels = I(levels_list),  # Use I() to keep lists intact
    stringsAsFactors = FALSE
  )

  # Get skim summary
  skimmed <- skimr::skim(data)

  # Merge labels with skimmed output
  skim_with_labels <- merge(labels_df, skimmed, by.x = "variable", by.y = "skim_variable", all.y = TRUE)

  # Remove columns that are completely NA
  skim_with_labels <- skim_with_labels[, colSums(is.na(skim_with_labels)) < nrow(skim_with_labels)]

  # Return the skimmed summary with labels and levels
  return(skim_with_labels)
}
