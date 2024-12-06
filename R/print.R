#' Print method for Tidyprompt (R6) objects
#'
#' This function prints a Tidyprompt object. It shows:
#' - The base prompt
#' - How many prompt wraps are applied
#' - The fully constructed prompt text after modifications
#'
#' @param x A Tidyprompt R6 object
#' @param ... Additional arguments (unused)
#'
#' @return The Tidyprompt object, invisibly.
#' @export
print.Tidyprompt <- function(x, ...) {
  cat(col_blue("<tidyprompt>\n"))

  line_prefix <- col_green("> ")

  format_with_prefix <- function(text, line_prefix) {
    lines <- unlist(strsplit(text, "\n", fixed = TRUE))
    paste0(line_prefix, lines, collapse = "\n")
  }

  # Get the prompt wraps
  wraps <- x$get_prompt_wraps()
  n_wraps <- length(wraps)

  if (n_wraps == 0) {
    cat(col_silver("The base prompt is not modified by prompt wraps:\n"))
    formatted_text <- format_with_prefix(x$get_base_prompt(), line_prefix)
    cat(formatted_text, "\n")
  } else if (n_wraps == 1) {
    cat(col_silver("The base prompt is modified by a prompt wrap, resulting in:\n"))
  } else if (n_wraps > 1) {
    cat(col_silver(
      paste("The base prompt is modified by", n_wraps, "prompt wraps, resulting in:\n")
    ))
  }

  if (n_wraps > 0) {
    full_text <- x$construct_prompt_text()
    formatted_text <- format_with_prefix(full_text, line_prefix)
    cat(formatted_text, "\n")
    cat(col_silver("Use 'x$prompt_wraps' to show the prompt wraps.\n"))
  }

  cat(col_silver("Use 'x$base_prompt' to show the base prompt text.\n"))
  cat(col_silver("Use 'x$construct_prompt_text()' to get the full prompt text.\n"))

  return(invisible(x))
}

col_blue <- function(text) {
  if (requireNamespace("cli", quietly = TRUE))
    return(cli::col_blue(text))
  text
}

col_green <- function(text) {
  if (requireNamespace("cli", quietly = TRUE))
    return(cli::col_green(text))
  text
}

col_silver <- function(text) {
  if (requireNamespace("cli", quietly = TRUE))
    return(cli::col_silver(text))
  text
}
