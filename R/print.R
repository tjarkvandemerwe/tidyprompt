#' Print method for [tidyprompt()] objects
#'
#' This function is a custom print method for displaying a [tidyprompt()] object.
#' A [tidyprompt()] typically contains a base prompt and may have additional prompt
#' wrappers that modify it. This function applies the modifications specified
#' in the wrapper functions and displays the resulting prompt in a structured
#' and visually clear manner.
#'
#' @param x A [tidyprompt()] object. The object should contain:
#'   \describe{
#'     \item{`base_prompt`}{A character string containing the base prompt text}
#'     \item{`prompt_wraps`}{A list containing wrapper functions that modify
#'     the base prompt}
#'   }
#' @param ... Additional arguments passed to `print.tidyprompt`
#' (not used; needs to be present in line with guidelines for generic functions)
#'
#' @details The `print.tidyprompt` function displays the base prompt and, if applicable,
#'   the modified prompt after applying the wrapper functions. The output is formatted
#'   with line breaks preserved and with colored text to distinguish metadata from
#'   the prompt content. This is done using the `cli` package to enhance
#'   readability, similar to the printing of tibbles in the tidyverse
#'
#' @return This function is used for its side effect of printing the prompt
#'   to the console. It returns `p` invisibly
#'
#' @examples
#' # Creating a simple tidyprompt object
#' prompt <- tidyprompt("What is the capital of France?")
#'
#' # Print the prompt object
#' print(prompt)
#'
#' # Adding some wrapper functions
#' prompt <- prompt |>
#'   prompt_wrap(modify_fn = \(x) paste0("Answer concisely: ", x))
#'
#' # Print the modified prompt object
#' print(prompt)
#'
#' @family tidyprompt
#'
#' @exportS3Method
#' @keywords internal
print.tidyprompt <- function(x, ...) {
  cat(cli::col_blue("<tidyprompt>\n"))

  line_prefix <- cli::col_green("> ")

  format_with_prefix <- function(text, line_prefix) {
    # Split the text by line breaks to preserve them
    lines <- unlist(strsplit(text, "\n", fixed = TRUE))
    # Apply prefix to each line and collapse them with line breaks
    formatted_text <- paste0(line_prefix, lines, collapse = "\n")
    return(formatted_text)
  }

  n_wraps <- length(x$prompt_wraps)
  if (n_wraps == 0) {
    cat(cli::col_silver("base prompt:\n"))  # Use gray for metadata
    formatted_text <- format_with_prefix(x$base_prompt, line_prefix)
    cat(formatted_text, "\n")
  }

  if (n_wraps == 1) {
    cat(cli::col_silver(paste("The base prompt is modified by a prompt wrap, resulting in:\n")))
  }

  if (n_wraps > 1) {
    cat(cli::col_silver(paste("The base prompt is modified by", n_wraps, "prompt wraps, resulting in:\n")))
  }

  if (n_wraps > 0) {
    full_text <- x |> construct_prompt_text()
    formatted_text <- format_with_prefix(full_text, line_prefix)
    cat(formatted_text, "\n")
    cat(cli::col_silver("Use '<tidyprompt>$prompt_wraps' to show the prompt wraps.\n"))
  }

  cat(cli::col_silver("Use '<tidyprompt>$base_prompt' to show the base prompt text.\n"))
  cat(cli::col_silver("Use '<tidyprompt> |> construct_prompt_text()' to get the full prompt text.\n"))

  return(invisible(x))
}
