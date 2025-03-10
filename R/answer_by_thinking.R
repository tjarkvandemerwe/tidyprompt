#' Extract the answer, reasoning, or both from a model's response
#'
#' This function processes responses from reasoning models and extracts either
#' the answer, the reasoning, or both based on the specified extraction mode.
#'
#' @param prompt A character string representing the model prompt.
#' @param tag A character string specifying the XML-like tag used to delimit
#'   the reasoning section. Default is "think".
#' @param extract A character string indicating what to extract: "answer" (default),
#'   "reasoning", or "both".
#'
#' @return A modified prompt object with an appropriate extraction function.
#' @export
answer_by_thinking <- function(
  prompt,
  tag = "think",
  extract = c("answer", "reasoning", "both")
) {
  extract <- match.arg(extract)

  extraction_fn <- switch(
    extract,
    answer = function(response) {
      stringr::str_replace(response, paste0("(?s).*</", tag, ">"), "")
    },
    reasoning = function(response) {
      stringr::str_match(response, paste0("(?s)<", tag, ">(.*?)</", tag, ">"))[,
        2
      ]
    },
    both = identity
  )

  prompt_wrap(
    prompt,
    modify_fn = NULL,
    extraction_fn = extraction_fn,
    validation_fn = NULL,
    type = "mode",
    name = "answer_by_thinking"
  )
}
