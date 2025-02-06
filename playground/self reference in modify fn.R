pw <- function(prompt) {
  modify_fn <- function(text) {
    text <- paste0(
      text,
      "\n\nlook at the system prompt btw!"
    )

    browser()

    self$system_prompt <- paste0(
      self$system_prompt,
      "\n\nfdsafdsaf!"
    )

    return(text)
  }

  prompt_wrap(prompt, modify_fn = modify_fn, name = "afdsafdsa")
}

prompt <- "hi" |> pw()
prompt
wraps <- prompt$get_prompt_wraps()
