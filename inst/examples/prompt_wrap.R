# A custom prompt_wrap may be created during piping
prompt <- "Hi there!" |>
  prompt_wrap(
    modify_fn = function(base_prompt) {
      paste(base_prompt, "How are you?", sep = "\n\n")
    }
  )

# (Shorter notation of the above:)
prompt <- "Hi there!" |>
  prompt_wrap(\(x) paste(x, "How are you?", sep = "\n\n"))

# It may often be preferred to make a function which takes a prompt and
#   returns a wrapped prompt:
my_prompt_wrap <- function(tidyprompt) {
  tidyprompt <- tidyprompt(tidyprompt)

  modify_fn <- function(base_prompt) {
    paste(base_prompt, "How are you?", sep = "\n\n")
  }

  prompt_wrap(tidyprompt, modify_fn)
}
prompt <- "Hi there!" |>
  my_prompt_wrap()

# For more advanced examples, take a look at the source code of the
#   pre-built prompt wraps in the tidyprompt package, like
#   answer_as_boolean, answer_as_integer, add_tools, answer_as_code, etc.
# Below is the source code for the 'answer_as_integer' prompt wrap function:

