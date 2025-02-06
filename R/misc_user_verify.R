#' Have user check the result of a prompt (human-in-the-loop)
#'
#' This function is used to have a user check the result of a prompt.
#' After evaluation of the prompt and applying prompt wraps,
#' the user is presented with the result and asked to accept or decline.
#' If the user declines, they are asked to provide feedback to the large
#' language model (LLM) so that the LLM can retry the prompt.
#'
#' @param prompt A single string or a [tidyprompt-class] object
#'
#' @return A [tidyprompt-class] with an added [prompt_wrap()] which
#' will add a check for the user to accept or decline the result of the prompt,
#' providing feedback if the result is declined
#'
#' @export
#'
#' @example inst/examples/user_verify.R
user_verify <- function(
  prompt
) {
  prompt <- tidyprompt(prompt)

  validation_fn <- function(response) {
    # Ask user input via console

    while (TRUE) {
      cli::cli_h3("Evaluation of tidyprompt resulted in:")
      cat(utils::capture.output(print(response)))
      cat("\n")

      cli::cli_h3("Accept or decline")
      cli::cli_alert_info("If {.strong satisfied}, type nothing")
      cli::cli_alert_info(
        "If {.strong not satisfied}, type feedback to the LLM"
      )

      feedback <- readline("Type: ")

      if (feedback == "") {
        cli::cli_alert_success("Result accepted")
        return(TRUE)
      }

      return(llm_feedback(feedback))
    }
  }

  prompt_wrap(
    prompt,
    validation_fn = validation_fn,
    type = "check"
  )
}
