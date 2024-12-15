#' @title
#' Have LLM check the result of a prompt (LLM-in-the-loop)
#'
#' @description
#' This function will wrap a prompt with a check for a LLM to accept or decline
#' the result of the prompt, providing feedback if the result is declined.
#' The evaluating LLM will be presented with the original prompt and the result
#' of the prompt, and will be asked to verify if the answer is satisfactory
#' (using chain of thought reasoning to arrive at a boolean decision). If
#' the result is declined, the chain of thought responsible for the decision
#' will be summarized and sent back to the original LLM that was asked to
#' evaluate the prompt, so that it may retry the prompt.
#'
#' Note that this function is experimental and, because it relies on chain of
#' thought reasoning by a LLM about the answer of another LLM, it may not
#' always provide accurate results and can dramatically increase the token cost of
#' evaluating a prompt.
#'
#' Note also that, currently, the evaluating LLM may attempt to evaluate criteria
#' which were already validated by prompt wraps, such as the format of the
#' response. This may lead to unexpected results. This may be fixed
#' in the future by only providing the base prompt and not the constructed
#' prompt to the evaluating LLM.
#'
#' @param prompt A single string or a [tidyprompt-class] object
#'
#' @param llm_provider A [llm_provider-class] object
#'  which will be used to verify the evaluation led to a satisfactory result.
#' If not provided, the same LLM provider as the prompt was originally
#'  evaluated with will be used
#'
#' @param max_words_feedback The maximum number of words allowed in the summary of
#'  why the result was declined.
#' This summary is sent back to the LLM originally asked to evaluate the prompt
#'
#' @return A [tidyprompt-class] with an added [prompt_wrap()] which
#'  will add a check for a LLM to accept or decline the result of the prompt,
#'  providing feedback if the result is declined
#'
#' @export
#'
#' @example inst/examples/llm_verify.R
llm_verify <- function(
    prompt,
    llm_provider = NULL,
    max_words_feedback = 50
) {
  prompt <- tidyprompt(prompt)

  # Verify & install provided LLM provider
  stopifnot(
    is.null(llm_provider) || inherits(llm_provider, "LlmProvider")
  )
  super_llm_provider <- llm_provider

  validation_fn <- function(response, llm_provider) {
    if (!is.null(super_llm_provider)) {
      llm_provider <- super_llm_provider
    }

    original_prompt <- self$construct_prompt_text()
    result_as_text <- utils::capture.output(print(response))

    satisfied <- glue::glue(
      ">>> An assistant was asked:\n\n",
      "{original_prompt}\n\n",
      ">>> The assistant answered:\n\n",
      "  ", paste(result_as_text, collapse = "\n  "), "\n\n",
      ">>> Please verify if the answer is satisfactory.",
      .trim = FALSE
    ) |>
      answer_as_boolean() |>
      answer_by_chain_of_thought() |>
      send_prompt(llm_provider, return_mode = "full")

    if (satisfied$response) return(TRUE)

    # Create summary of why the response was declined
    feedback <- glue::glue(
      "An assistant's answer was declined by another assistant.",
      "The declining assistant provided their chain of thought:\n\n",
      satisfied$chat_history_clean |>
        dplyr::select(c("role", "content")) |>
        df_to_string(),
      "\n\n",
      "Summarize why the result was declined."
    ) |>
      answer_as_text(max_words = max_words_feedback) |>
      send_prompt(llm_provider)

    return(llm_feedback(paste0(
      "Your answer was rejected for this reason:\n\n", feedback
    )))
  }

  prompt_wrap(
    prompt,
    validation_fn = validation_fn,
    type = "check"
  )
}
