#' @title Set ReAct mode for a prompt
#'
#' @description This function enables ReAct mode for the evaluation of a prompt
#' or a [tidyprompt()]. In ReAct mode, the large language model (LLM) is asked to think step by step,
#' each time detailing a thought, action, and observation, to eventually arrive at
#' a final answer. It is hypothesized that this may increase LLM performance
#' at solving complex tasks. ReAct mode is inspired by the method described in
#' Yao et al. (2022).
#'
#' @details Please note that ReAct mode may be most useful if in combination
#' with tools that the LLM can use. See, for example, 'add_tools()' for
#' enabling R function calling, or, for example, 'answer_as_code()' with
#' 'output_as_tool = TRUE' for enabling R code evaluation as a tool.
#'
#' @param prompt A single string or a [tidyprompt()] object
#' @param extract_from_finish_brackets A logical indicating whether the final answer
#' should be extracted from the text inside the "FINISH[...]" brackets
#' @param extraction_lenience A logical indcating whether the extraction function should be lenient.
#' If TRUE, the extraction function will attempt to extract the final answer
#' even if it cannot be extracted from within the brackets, by extracting
#' everything after the final occurence of 'FINISH' (if present). This may
#' be useful for smaller LLMs which may not follow the output format
#' as strictly
#'
#' @return A [tidyprompt()] with an added [prompt_wrap()] which will ensure
#' that the LLM follows the ReAct mode in answering the prompt
#'
#' @export
#'
#' @example inst/examples/answer_by_react.R
#'
#' @references
#' Yao, S., Wu, Y., Cheung, W., Wang, Z., Narasimhan, K., & Kong, L. (2022).
#' ReAct: Synergizing Reasoning and Acting in Language Models.
#' <doi:10.48550/arXiv.2210.03629>
#'
#' @seealso [answer_using_tools()] [answer_as_code()]
#'
#' @family pre_built_prompt_wraps
#' @family answer_by_prompt_wraps
answer_by_react <- function(
    prompt,
    extract_from_finish_brackets = TRUE,
    extraction_lenience = TRUE
) {

  # Define modification/extraction/validation functions:
  modify_fn <- function(original_prompt_text) {
    new_prompt <- glue::glue(
      "You are given a user's prompt.
      To answer the user's prompt, you need to think step by step, take an action if needed, and then return a final answer.

      ----- START OF USER'S PROMPT -----
      {original_prompt_text}
      ----- END OF USER'S PROMPT -----

      Use the following structure:
        Thought: <describe your thought process>
        Action: <if needed, describe the action you take (e.g., look up information)>
        Observation: <describe the result or observation from the action>
      (Repeat Thought -> Action -> Observation as necessary)

      When you are done, you must type:
        FINISH[<put here your final answer to the user's prompt>]

      Ensure your final answer aligns with your reasoning and observations."
    )

    return(new_prompt)
  }

  extraction_fn <- function(llm_response) {
    if (!extract_from_finish_brackets)
      return(llm_response)
    extraction_fn_finish(llm_response, extraction_lenience)
  }

  prompt_wrap(
    prompt,
    modify_fn, extraction_fn,
    type = "mode", name = "answer_by_react"
  )
}
