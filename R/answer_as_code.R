#' Instruct LLM to answer a prompt with R code
#'
#' This function adds a prompt wrap to a tidyprompt object that instructs the
#' LLM to answer the prompt with R code. There are various options to customize
#' the behavior of this prompt wrap, concerning the evaluation of the R code,
#' the packages that may be used, the objects that already exist in the R
#' session, and if the console output that should be sent back to the LLM. For
#' the evaluation of the R code, the 'callr' package is required. Please note:
#' automatic evaluation of generated R code may be dangerous to your system;
#' use this function with caution.
#'
#' @param prompt A character string or a tidyprompt object
#' @param add_text Character string which will be added to the prompt text,
#' informing the LLM that they must code in R to answer the prompt.
#' @param pkgs_to_use A character vector of package names that may be used
#' in the R code that the LLM will generate
#' @param evaluate_code Logical indicating whether the R code should be
#' evaluated. If TRUE, the R code will be evaluated in an R session.
#' @param evaluation_session An r_session object (from the callr package) to
#' evaluate the R code. If NULL, a new r_session object will be created.
#' @param list_packages Logical indicating whether the LLM should be informed
#' about the packages that may be used in the R code
#' @param list_objects Logical indicating whether the LLM should be informed
#' about the objects that already exist in the R session, including their types
#' @param send_back_output Logical indicating whether the console output of the
#' evaluated R code should be sent back to the LLM. If so, the LLM can decide
#' if they can answer the prompt or if they need to modify their R code. Once
#' the LLM does not provide new R code (i.e., the prompt is being answered)
#' the extraction of this prompt wrap will end
#'
#' @return A tidyprompt object with the new prompt wrap added to it
#' @export
#' @examples
#' \dontrun{
#' "Hi, what is 24314*24433?" |>
#'  answer_as_code() |>
#'  answer_as_integer() |>
#'  answer_by_chain_of_thought() |>
#'  send_prompt()
#'
#'  paste0(
#'  "Using the built-in airquality dataset in R,",
#'  " calculate the average Ozone level for days in May with temperatures",
#'  " above 80°F."
#'  ) |>
#'   answer_as_integer() |>
#'   answer_as_code(pkgs_to_use = c("dplyr")) |>
#'   answer_by_chain_of_thought() |>
#'   send_prompt()
#' }
answer_as_code <- function(
    prompt,
    add_text = "You must code in the programming language 'R' to answer this prompt.",
    pkgs_to_use = c(),
    evaluate_code = TRUE,
    evaluation_session = NULL,
    list_packages = TRUE,
    list_objects = TRUE,
    send_back_output = TRUE
) {
  prompt <- tidyprompt(prompt)

  if (evaluate_code & !requireNamespace("callr", quietly = TRUE)) {
    stop("The 'callr' package is required to evaluate R code.")
  }

  if (!evaluate_code & !is.null(evaluation_session)) {
    stop("evaluation_session must be NULL if evaluate_code is FALSE")
  }
  if (is.null(evaluation_session) & evaluate_code) {
    evaluation_session <- callr::r_session$new()
  }
  if (evaluate_code & !inherits(evaluation_session, "r_session")) {
    stop(paste0(
      "evaluation_session must be an r_session object if evaluate_code is TRUE"
    ))
  }
  if (!evaluate_code & send_back_output) {
    stop("send_back_output must be FALSE if evaluate_code is FALSE")
  }

  if (evaluate_code) {
    loaded_pkgs <- evaluation_session$run(function(pkgs_to_use) {
      for (pkg_name in pkgs_to_use) {
        library(pkg_name, character.only = TRUE)
      }
      session_info <- utils::sessionInfo()
      loaded_pkgs <- names(session_info$otherPkgs)
      loaded_pkgs
    }, args = list(pkgs_to_use = pkgs_to_use))
  } else {
    loaded_pkgs <- pkgs_to_use
  }

  modify_fn <- function(original_prompt_text) {
    new_text <- glue::glue(
      "{original_prompt_text}\n\n",
      "{add_text}"
    )

    if (list_packages & length(loaded_pkgs) > 0) {
      new_text <- glue::glue(
        "{new_text}\n",
        "You can use functions from these packages: ",
        "{loaded_pkgs |> paste(collapse = ', ')}."
      )
    }

    if (list_objects & !is.null(evaluation_session)) {
      objects <- evaluation_session$run(function() {
        objects <- ls()
        object_types <- sapply(objects, function(obj) class(get(obj)))
        data.frame(Object_name = objects, Type = object_types)
      })

      if (nrow(objects) > 0) {
        new_text <- glue::glue(
          "{new_text}\n",
          "These objects already exist in the R session:\n\n",
          "{objects |> df_to_string()}.\n\n",
          "Do not define these objects in your R code."
        )

        if (send_back_output) {
          new_text <- glue::glue(
            "{new_text}\n",
            "If needed, you may first write R code to better understand ",
            " these objects."
          )
        }
      }
    }

    if (evaluate_code) {
      new_text <- glue::glue(
        "{new_text}\n",
        "The R code should produce console output that answers the prompt."
      )
    }

    if (send_back_output) {
      new_text <- glue::glue(
        "{new_text}\n",
        "The console output of your R code will be sent back to you.",
        " With the console output, decide if you can answer the prompt or if",
        " you need to modify your R code. Provide R code until you can",
        " answer the prompt."
      )
    }

    return(new_text)
  }

  extraction_fn <- function(x) {
    extracted_code <- extract_r_code_from_string(x)

    if (length(extracted_code) == 0) {
      if (send_back_output) {
        return(x)
      }

      return(create_llm_feedback(paste0(
        "No R code detected. You must provide R code",
        " between ```r and ```."
      )))
    }

    # Check if the R code is valid
    parsed_code <- tryCatch(parse(text = extracted_code), error = function(e) e)
    if (inherits(parsed_code, "error")) {
      return(create_llm_feedback(glue::glue(
        "Invalid R code detected:\n",
        "    {parsed_code$message}\n",
        "Please provide syntactically correct R code."
      )))
    }

    if (!evaluate_code) {
      return(parsed_code)
    }

    output <- evaluation_session$run_with_output(function(r_code) {
      eval(parse(text = r_code))
    }, args = list(parsed_code))

    # Check if errors occured during execution
    if (!is.null(output$error)) {
      return(create_llm_feedback(glue::glue(
        "An error occurred while executing the R code:\n",
        "    {output$error}"
      )))
    }

    if (output$stdout == "") {
      return(create_llm_feedback(glue::glue(
        "The R code did not produce any console output.",
        " Please provide R code that produces console output."
      )))
    }

    if (send_back_output) {
      return(create_llm_feedback(glue::glue(
        "R code executed. Console output:\n",
        "    {output$stdout}"
      )))
    }

    return(output$stdout)
  }

  type <- "unspecified"
  if (send_back_output) {
    type <- "tool"
  }

  prompt_wrap(prompt, modify_fn, extraction_fn, type = type)
}



#' Helper function to extract R code from a string
#'
#' This function extracts R code from a string by matching all content between
#' '```r' and '```'.
#'
#' @param input_string A string containing R code, typically a response
#' from an LLM
#'
#' @return A character vector containing the extracted R code
#' @export
extract_r_code_from_string <- function(input_string) {
  # Use regular expression to match all content between ```r and ```, with case-insensitive matching
  matches <- gregexpr("(?s)```[rR]\\s*(.*?)\\s*```", input_string, perl = TRUE)
  extracted_code <- regmatches(input_string, matches)

  # Remove the ```r and ``` wrappers (works for both lowercase and uppercase R)
  extracted_code <- lapply(extracted_code, function(x) {
    sub("(?s)```[rR]\\s*(.*?)\\s*```", "\\1", x, perl = TRUE)
  })

  return(unlist(extracted_code))
}
