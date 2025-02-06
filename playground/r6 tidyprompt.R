tp <- R6::R6Class(
  "tp",

  public = list(
    prompt = NULL,
    modify_fn = NULL,
    my_other_field = " hi from tp object",

    initialize = function(prompt = "Hi!") {
      self$prompt <- prompt
    },

    construct_prompt = function() {
      prompt <- self$prompt

      if (!is.null(self$modify_fn)) {
        # Add `self` to the environment of `modify_fn`
        environment(self$modify_fn) <- list2env(
          list(self = self),
          parent = environment(self$modify_fn)
        )
        prompt <- self$modify_fn(prompt)
      }

      return(prompt)
    }
  )
)

tpi <- tp$new()
# tpi$modify_fn <- function(prompt) {
#   paste0(prompt, " How are you?")
# }
# tpi$construct_prompt()

pw <- function(tpi, text = " hi from wrap function env!") {
  tpi <- tpi$clone()

  tpi$modify_fn <- function(prompt) {
    paste0(prompt, text, self$my_other_field)
  }

  return(tpi)
}

tpi <- pw(tpi)
tpi$construct_prompt()
