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
#' in the R code that the LLM will generate. If evaluating the R code, these
#' will be pre-loaded in the R session.
#' @param evaluate_code Logical indicating whether the R code should be
#' evaluated. If TRUE, the R code will be evaluated in a new R session
#' (using the 'callr' package).
#' @param evaluation_session A pre-existing r_session object (from the 'callr' package)
#' to evaluate the R code (e.g., with certain objects loaded). If NULL, a
#' new r_session object will be created.
#' @param list_packages Logical indicating whether the LLM should be informed
#' about the packages that may be used in the R code (if TRUE, a list of the
#' loaded packages will be shown in the initial prompt)
#' @param list_objects Logical indicating whether the LLM should be informed
#' about the objects that already exist in the R session (if TRUE, a list of the objects
#' plus their types will be shown in the initial prompt)
#' @param skim_dataframes Logical indicating whether the LLM should be informed
#' about the structure of dataframes that already exist in the R session (if TRUE,
#' a skim summary of each dataframe will be shown in the initial prompt)
#' @param output_as_tool Logical indicating whether the console output of the
#' evaluated R code should be sent back to the LLM, meaning the LLM will use
#' R code as a tool to formulate an answer to the prompt. If TRUE, the LLM
#' can decide if they can answer the prompt with the output, or if they need to modify
#' their R code. Once the LLM does not provide new R code (i.e., the prompt is being answered)
#' this prompt wrap will end (it will continue for as long as the LLM provides R code).
#' @param return_mode Character string indicating the return mode. One of
#' 'full', 'code', 'console', 'object', 'formatted_output', or 'llm_answer'.
#' If 'full', the function will return a list with the original LLM answer,
#' the extracted R code, and (if evaluated) the output of the R code.
#' If 'code', the function will return the extracted R code. If 'console', the
#' function will return the console output of the evaluated R code.
#' If 'object', the function will return the object produced by the evaluated R code.
#' If 'formatted_output', the function will return a formatted string with the
#' extracted R code, its console output, and a print of the last object (identical
#' to how it would be presented to the LLM if 'output_as_tool' is TRUE).
#' If 'llm_answer', the function will return only the original LLM answer.
#' When choosing 'console' or 'object', an additional instruction will be added to
#' the prompt text to inform the LLM about the expected output of the R code.
#'
#' @return A tidyprompt object with the new prompt wrap added to it
#' @export
answer_as_code <- function(
    prompt,
    add_text = "You must code in the programming language 'R' to answer this prompt.",
    pkgs_to_use = c(),
    evaluate_code = TRUE,
    evaluation_session = NULL,
    list_packages = TRUE,
    list_objects = TRUE,
    skim_dataframes = TRUE,
    output_as_tool = FALSE,
    return_mode = c("full", "code", "console", "object", "formatted_output", "llm_answer")
) {
  prompt <- tidyprompt(prompt)

  ## Validate settings

  if (evaluate_code & !requireNamespace("callr", quietly = TRUE))
    stop("The 'callr' package is required to evaluate R code.")
  if (!evaluate_code)
    evaluation_session <- NULL
  if (!evaluate_code & output_as_tool)
    output_as_tool <- FALSE
  if (output_as_tool)
    return_mode <- "llm_answer"
  if (!evaluate_code & return_mode %in% c("console", "object", "formatted_output"))
    stop("The return mode must be 'full', 'code', or 'llm_answer' if 'evaluate_code' is FALSE.")

  return_mode <- match.arg(return_mode)


  ## Validate evaluation_session & load packages

  if (evaluate_code) {
    if (is.null(evaluation_session)) {
      evaluation_session <- callr::r_session$new() # Make new r_session
    } else if (!inherits(evaluation_session, "r_session")) {
      stop(paste0(
        "evaluation_session must be an r_session object"
      ))
    }

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


  ## Define modify_fn which will add information about the setting
  ##   in which R code can be generated

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

    new_text <- glue::glue(
      "{new_text}\n",
      "You may not install or load any additional packages."
    )

    if (list_objects & !is.null(evaluation_session)) {
      objects <- evaluation_session$run(function() {
        objects <- ls(envir = parent.env(environment()))
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

        if (skim_dataframes) {
          dataframes <- objects$Object_name[objects$Type == "data.frame"]
          for (df_name in dataframes) {
            df <- evaluation_session$run(function(df_name) {
              df <- get(df_name)
              df
            }, args = list(df_name = df_name))

            new_text <- glue::glue(
              "{new_text}\n\n",
              "Summary of the dataframe '{df_name}':\n",
              "{df |> skim_with_labels_and_levels() |> df_to_string()}\n\n"
            )
          }
        }

        if (output_as_tool) {
          new_text <- glue::glue(
            "{new_text}\n",
            "If you need more information about these objects,",
            " you can call R functions to describe them."
          )
        }
      }
    }

    if (evaluate_code & return_mode == "console") {
      new_text <- glue::glue(
        "{new_text}\n",
        "The R code should produce console output that answers the prompt."
      )
    }
    if (evaluate_code & return_mode == "object") {
      new_text <- glue::glue(
        "{new_text}\n",
        "The R code should produce an object that answers the prompt."
      )
    }

    if (output_as_tool) {
      new_text <- glue::glue(
        "{new_text}\n",
        "The console output of your R code will be sent back to you.",
        " Use print() on all objects or values that you need to see.",
        " You can not view plots, all output must be text-based.",
        " After you get console output from me, decide if you can answer the prompt or if",
        " you need to modify your R code. When you can formulate your final answer,",
        " do not provide any R code in it."
      )
    }

    return(new_text)
  }


  ## Define extraction_fn which will extract R code from the response
  ##   and handle it according to the settings of this function

  extraction_fn <- function(x) {
    return_list <- list()
    return_list$llm_answer <- x

    extracted_code <- extract_r_code_from_string(x)

    if (length(extracted_code) == 0) {
      if (output_as_tool) {
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
    return_list$code <- parsed_code

    if (!evaluate_code) {
      return(parsed_code)
    }

    clone_session <- evaluation_session$clone() # Reset the session everytime
    output <- clone_session$run_with_output(function(r_code) {
      eval(parse(text = r_code))
    }, args = list(parsed_code))

    # Check if errors occured during execution
    if (!is.null(output$error)) {
      return(create_llm_feedback(glue::glue(
        "An error occurred while executing the R code:\n",
        "    {output$error}"
      )))
    }

    # Check if the code produced any relevant output
    if (output$stdout == "" & return_mode == "console") {
      return(create_llm_feedback(glue::glue(
        "The R code did not produce any console output.",
        " Please provide R code that produces console output."
      )))
    }
    if (is.null(output$result) & return_mode == "object") {
      return(create_llm_feedback(glue::glue(
        "The R code did not produce an object.",
        " Please provide R code that produces an object."
      )))
    }
    if (is.null(output$stdout) & is.null(output$result)) {
      return(create_llm_feedback(glue::glue(
        "The R code did not produce any output.",
        " Please provide R code that produces output."
      )))
    }

    return_list$output <- output
    return_list$formatted_output <- glue::glue(
      "--- R code: ---\n",
      "{extracted_code |> paste(collapse = \"\\n\")}\n\n",
      "--- Console output: ---\n",
      "{
          if (is.null(output$stdout) || output$stdout == \"\") {
            \"No console output produced.\"
          } else {
            output$stdout |>
            paste(collapse = \"\\n\") |>
            stringr::str_trunc(1000) |>
            print()
          }
        }\n\n",
      "--- Last object: ---\n",
      "{
          if (is.null(output$result)) {
            \"No object produced.\"
          } else {
            output$result |>
            paste(collapse = \"\\n\") |>
            stringr::str_trunc(100) |>
            print()
          }
        }"
    )

    if (output_as_tool) {
      return(create_llm_feedback(return_list$formatted_output, tool_result = TRUE))
    }

    if (return_mode == "full")
      return(return_list)
    if (return_mode == "code")
      return(return_list$code)
    if (return_mode == "console")
      return(return_list$output$stdout)
    if (return_mode == "object")
      return(return_list$output$result)
    if (return_mode == "formatted_output")
      return(return_list$formatted_output)
    if (return_mode == "llm_answer")
      return(x)

    return(output$stdout)
  }


  ## If we are sending back output, we can consider this wrapper a tool

  type <- "unspecified"
  if (output_as_tool) {
    type <- "tool"
  }


  ## Finally, wrap the prompt with the new prompt wrap

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
