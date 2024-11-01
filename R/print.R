#' Print Method for tidyprompt Objects
#'
#' This function is a custom print method for displaying a `tidyprompt` object.
#' A `tidyprompt` typically contains a base prompt and may have additional prompt
#' wrappers that modify it. This function applies the modifications specified
#' in the wrapper functions and displays the resulting prompt in a structured
#' and visually clear manner.
#'
#' @param x A `tidyprompt` object. The object should contain:
#'   \describe{
#'     \item{`base_prompt`}{A character string containing the base prompt text.}
#'     \item{`prompt_wraps`}{A list containing wrapper functions that modify
#'     the base prompt.}
#'   }
#' @param ... Additional arguments passed to `print.tidyprompt`
#' (not used; needs to be present in line with guidelines for generic functions).
#'
#' @details The `print.tidyprompt` function displays the base prompt and, if applicable,
#'   the modified prompt after applying the wrapper functions. The output is formatted
#'   with line breaks preserved and with colored text to distinguish metadata from
#'   the prompt content. This is done using the `crayon` package to enhance
#'   readability, similar to the printing of tibbles in the tidyverse.
#'
#' @return This function is used for its side effect of printing the prompt
#'   to the console. It returns `p` invisibly.
#'
#' @examples
#' \dontrun{
#'   # Creating a simple tidyprompt object
#'   prompt_obj <- tidyprompt("What is the capital of France?")
#'
#'   # Print the prompt object
#'   print(prompt_obj)
#'
#'   # Adding some wrapper functions
#'   prompt_obj <- prompt_obj |>
#'     prompt_wrap(modify_fn = \(x) paste0("Answer concisely: ", x))
#'
#'   # Print the modified prompt object
#'   print(prompt_obj)
#' }
#'
#' @importFrom crayon blue green silver
#' @exportS3Method print tidyprompt
print.tidyprompt <- function(x, ...) {
  cat(crayon::blue("<tidyprompt>\n"))

  line_prefix <- crayon::green("> ")

  format_with_prefix <- function(text, line_prefix) {
    # Split the text by line breaks to preserve them
    lines <- unlist(strsplit(text, "\n", fixed = TRUE))
    # Apply prefix to each line and collapse them with line breaks
    formatted_text <- paste0(line_prefix, lines, collapse = "\n")
    return(formatted_text)
  }

  n_wraps <- length(x$prompt_wraps)
  if (n_wraps == 0) {
    cat(crayon::silver("base prompt:\n"))  # Use gray for metadata
    formatted_text <- format_with_prefix(x$base_prompt, line_prefix)
    cat(formatted_text, "\n")
  }

  if (n_wraps == 1) {
    cat(crayon::silver(paste("The base prompt is modified by a wrapper function, resulting in:\n")))
  }

  if (n_wraps > 1) {
    cat(crayon::silver(paste("The base prompt is modified by", n_wraps, "wrapper functions, resulting in:\n")))
  }

  if (n_wraps > 0) {
    full_text <- x |> construct_prompt_text()
    formatted_text <- format_with_prefix(full_text, line_prefix)
    cat(formatted_text, "\n")
    cat(crayon::silver("Use <prompt>$prompt_wraps to show the wrapper functions.\n"))
  }

  cat(crayon::silver("Use <prompt>$base_prompt to show the base prompt.\n"))

  return(invisible(x))
}
