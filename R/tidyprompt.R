#' Methods to create, construct, and empower prompt objects
#'
#' A tidyprompt is an object which contains an initial prompt text
#' and a list of [prompt_wrap()]. The prompt wraps contain functions
#' which modify the prompt text in some way, and, once the LLM response
#' to the prompt is given, apply extraction and validation to the response.
#' Using a [tidyprompt()] and [prompt_wrap()] objects allows for easy
#' chaining of modifications and empowerements to a prompt, ensuring
#' that the LLM output will meet the desired criteria. The [tidyprompt()]
#' object offers many possibilities, such as structured and validated output,
#' LLM function calling, and LLM code generation and evaluation.
#'
#' @param input Input to prompt. If a character string is passed,
#' a new prompt object will be created with that character string as the base prompt.
#'
#' @return A prompt object (or an error if an unsuitable input is provided)
#'
#' @export
#'
#' @seealso [prompt_wrap()] [send_prompt()]
#' @family tidyprompt
tidyprompt <- function(input) {
  UseMethod("tidyprompt")
}



#' Default method to create a tidyprompt object
#'
#' Is called when the input is not a character string or a tidyprompt object.
#'
#' @param input Input to create_tidyprompt
#'
#' @return An error message stating that input type is not suitable
#'
#' @exportS3Method tidyprompt default
#' @keywords internal
tidyprompt.default <- function(input) {
  stop(paste0(
    "Input (the base prompt) to tidyprompt() must be a character string"
  ))
}



#' Method to create a tidyprompt object from a character string
#'
#' @param input Input to create_tidyprompt; the base prompt
#'
#' @return A tidyprompt object
#'
#' @exportS3Method tidyprompt character
#' @keywords internal
tidyprompt.character <- function(input) {
  if (length(input) != 1)
    stop("Input (the base prompt) must be length 1")

  tidyprompt <- structure(
    list(
      base_prompt = input,
      prompt_wraps = list()
    ),
    class = "tidyprompt"
  )

  return(tidyprompt)
}



#' Validate tidyprompt
#'
#' @param input A tidyprompt object
#'
#' @return the input object if the tidyprompt is valid,
#'  otherwise an error is thrown
#'
#' @exportS3Method tidyprompt tidyprompt
#'
#' @keywords internal
tidyprompt.tidyprompt <- function(input) {
  if (!is.list(input))
    stop("The tidyprompt object must be a list")

  if (is.null(names(input)))
    stop("The tidyprompt object must have names")

  if (!"base_prompt" %in% names(input))
    stop("The tidyprompt object must have $base_prompt")

  if (
    !is.character(input$base_prompt) |
    input$base_prompt |> length() != 1
  )
    stop("$base_prompt must be a single string")

  if ("prompt_wraps" %in% names(input)) {
    if (!is.list(input$prompt_wraps))
      stop("$prompt_wraps must be a list")

    if (!all(sapply(input$prompt_wraps, function(x) inherits(x, "prompt_wrap"))))
      stop(paste0(
        "All elements of $prompt_wraps must be of class `prompt_wrap`.",
        " Create a `prompt_wrap` object with the prompt_wrap() function"
      ))
  }

  return(input)
}



#' Validate tidyprompt: returns TRUE if a valid tidyprompt object, otherwise FALSE
#'
#' @param tidyprompt A tidyprompt object
#'
#' @return Logical indicating whether the input is a valid tidyprompt object
#'
#' @export
#'
#' @family tidyprompt
is_tidyprompt <- function(tidyprompt) {
  if (!inherits(tidyprompt, "tidyprompt"))
    return(FALSE)

  tryCatch({
    invisible(tidyprompt(tidyprompt))
    TRUE
  }, error = function(e) {
    FALSE
  })
}



#' Get base prompt from tidyprompt
#'
#' @param tidyprompt A tidyprompt object
#'
#' @return The base prompt from the tidyprompt
#'
#' @export
#'
#' @family tidyprompt
get_base_prompt <- function(tidyprompt) {
  tidyprompt <- tidyprompt(tidyprompt)

  base_prompt <- tidyprompt$base_prompt

  return(base_prompt)
}



#' Get prompt wraps from tidyprompt
#'
#' @param tidyprompt A tidyprompt object
#' @param order The order in which to return the prompt wraps. Options are "default",
#' "modification", and "evaluation".
#'  "default" returns the prompt wraps in the order they were added.
#'  "modification" returns the prompt wraps in the order of "unspecified", "break", "mode", "tool";
#' this is the order in which the prompt wraps are applied to the base prompt when constructing the prompt text.
#'  "evaluation" returns the prompt wraps in the order of "tool", "mode", "break", "unspecified";
#'  this is the order in which the prompt wraps are applied to the LLM response when extracting and validating.
#'
#' @return A list of prompt wraps from the tidyprompt
#'
#' @export
#'
#' @family tidyprompt
get_prompt_wraps <- function(
    tidyprompt,
    order = c(
      "default",
      "modification",
      "evaluation"
    )
) {
  tidyprompt <- tidyprompt(tidyprompt)
  order <- match.arg(order)

  prompt_wraps <- tidyprompt$prompt_wraps

  if (order == "default")
    return(prompt_wraps)

  if (length(prompt_wraps) == 0)
    return(list())

  t_unspecified <-
    prompt_wraps[sapply(prompt_wraps, function(x) x$type == "unspecified")]
  t_mode <-
    prompt_wraps[sapply(prompt_wraps, function(x) x$type == "mode")]
  t_tool <-
    prompt_wraps[sapply(prompt_wraps, function(x) x$type == "tool")]
  t_break <-
    prompt_wraps[sapply(prompt_wraps, function(x) x$type == "break")]

  if (order == "modification")
    return(c(t_unspecified, t_break, t_mode, t_tool))

  if (order == "evaluation")
    return(c(t_tool, t_mode, t_break, t_unspecified))
}



#' Construct prompt text from a tidyprompt object
#'
#' @param tidyprompt A tidyprompt object
#' @param llm_provider An optional LLM provider object. In some cases
#' this may affect how the prompt text is constructed (e.g.,
#' the [answer_as_json()] prompt_wrap may not include a schema in the
#' prompt text but an OpenAI API, but may include it for other types).
#' The llm_provider will be passed to the modify_fn functions of the prompt wraps
#'
#' @return The prompt text constructed from the tidyprompt object
#'
#' @export
#'
#' @family tidyprompt
construct_prompt_text <- function(tidyprompt, llm_provider = NULL) {
  tidyprompt <- tidyprompt(tidyprompt)

  prompt_text <- get_base_prompt(tidyprompt)
  prompt_wraps <- get_prompt_wraps(tidyprompt, order = "modification")

  if (length(prompt_wraps) > 0) {
    for (i in 1:length(prompt_wraps)) {
      if (is.null(prompt_wraps[[i]]$modify_fn))
        next
      prompt_text <- prompt_wraps[[i]]$modify_fn(prompt_text, llm_provider)
    }
  }
  return(prompt_text)
}
