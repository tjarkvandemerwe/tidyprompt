# Example usage within an extraction function similar to the one in 'quit_if()':
extraction_fn <- function(x) {
  quit_detect_regex <- "NO ANSWER"

  if (grepl(quit_detect_regex, x)) {
      return(llm_break(
        object_to_return = NULL,
        success = TRUE
      ))
  }

  return(x)
}

\dontrun{
  result <- "How many months old is the cat of my uncle?" |>
    answer_as_integer() |>
    prompt_wrap(
      modify_fn = function(prompt) {
        paste0(
          prompt, "\n\n",
          "Type only 'NO ANSWER' if you do not know."
        )
      },
      extraction_fn = extraction_fn,
      type = "break"
    ) |>
    send_prompt()
  result
  # NULL
}
