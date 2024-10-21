#' Set the llm_provider for a prompt
#'
#' @param prompt A single character string or a prompt_list object
#' @param llm_provider An object of class llm_provider
#'
#' @return A prompt_list object with the llm_provider set as an attribute
#' @export
set_llm_provider <- function(prompt, llm_provider) {
  prompt <- prompt(prompt)

  if (!inherits(llm_provider, "llm_provider"))
    stop("llm_provider must be an object of class llm_provider")

  prompt$llm_provider <- llm_provider

  return(prompt)
}



