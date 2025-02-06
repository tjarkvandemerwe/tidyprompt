#' Enable LLM to draft and execute SQL queries on a database
#'
#' @param prompt A single string or a [tidyprompt()] object
#'
#' @param add_text Single string which will be added to the prompt text,
#' informing the LLM that they must use SQL to answer the prompt
#'
#' @param conn A DBIConnection object to the SQL database
#'
#' @param list_tables Logical indicating whether to list tables available in the database
#' in the prompt text
#'
#' @param describe_tables Logical indicating whether to describe the tables available in the database
#' in the prompt text. If TRUE, the columns of each table will be listed
#'
#' @param evaluate_code Logical indicating whether to evaluate the SQL code.
#' If TRUE, the SQL code will be executed on the database and the results will be returned.
#' Use with caution, as this allows the LLM to execute arbitrary SQL code
#'
#' @param output_as_tool Logical indicating whether to return the output as a tool result.
#' If TRUE, the output of the SQL query will be sent back to the LLM as a tool result.
#' The LLM can then provide a final answer or try another query. This can
#' continue until the LLM provides a final answer without any SQL code
#'
#' @param return_mode Character string indicating the return mode. Options are:
#' \itemize{
#'  \item "full": Return a list containing the SQL code, output, and formatted output
#'  \item "code": Return only the SQL code
#'  \item "object": Return only the query result object
#'  \item "formatted_output": Return the formatted output: a string detailing the SQL code
#'  and query result object.This is identical to how the LLM would see the output when
#'  output_as_tool is TRUE
#'  \item "llm_answer": Return the LLM answer. If output as tool is TRUE,
#'  the return mode will always be "llm_answer" (since the LLM uses SQL to
#'  provide a final answer)
#'  }
#'
#' @return A [tidyprompt()] with an added [prompt_wrap()] which will ensure
#' that the LLM will use SQL to answer the prompt
#'
#' @export
#'
#' @example inst/examples/answer_using_sql.R
#'
#' @family pre_built_prompt_wraps
#' @family answer_using_prompt_wraps
answer_using_sql <- function(
  prompt,
  add_text = paste0(
    "You must code in SQL to answer this prompt.",
    " You must provide all SQL code between ```sql and ```.",
    "\n\n",
    "Never make assumptions about the possible values in the tables.\n",
    "Instead, execute SQL queries to retrieve information you need."
  ),
  conn,
  list_tables = TRUE,
  describe_tables = TRUE,
  evaluate_code = FALSE,
  output_as_tool = FALSE,
  return_mode = c(
    "full",
    "code",
    "object",
    "formatted_output",
    "llm_answer"
  )
) {
  # Validate settings
  return_mode <- match.arg(return_mode)

  stopifnot(
    is.character(add_text),
    length(add_text) == 1,
    inherits(conn, "DBIConnection"),
    is.logical(list_tables),
    is.logical(describe_tables),
    is.logical(evaluate_code),
    is.logical(output_as_tool)
  )

  if (!requireNamespace("DBI", quietly = TRUE))
    stop(
      "'DBI' package is required; please install it using 'install.packages(\"DBI\")"
    )

  if (!evaluate_code & output_as_tool) output_as_tool <- FALSE
  if (output_as_tool) return_mode <- "llm_answer"
  if (
    !evaluate_code & return_mode %in% c("console", "object", "formatted_output")
  )
    stop(
      "The return mode must be 'full', 'code', or 'llm_answer' if 'evaluate_code' is FALSE"
    )

  # Retrieve list of tables if needed
  tables <- character(0)
  if (list_tables) {
    tables <- DBI::dbListTables(conn)
  }

  # If describe_tables is TRUE, we will attempt to fetch their columns
  describe_text <- ""
  if (list_tables && describe_tables && length(tables) > 0) {
    describe_info <- lapply(tables, function(tbl) {
      fields <- tryCatch(
        DBI::dbListFields(conn, tbl),
        error = function(e) character(0)
      )
      fields_str <- if (length(fields) > 0) {
        paste0("  Columns: ", paste(fields, collapse = ", "))
      } else {
        "  No columns could be retrieved."
      }
      paste0("- ", tbl, "\n", fields_str)
    })
    describe_text <- paste0(
      "\n\nTable descriptions:\n",
      paste(describe_info, collapse = "\n\n")
    )
  }

  # Define modify_fn which adds info about the SQL environment
  modify_fn <- function(original_prompt_text) {
    new_text <- glue::glue(
      "{original_prompt_text}\n\n",
      "{add_text}"
    )

    if (list_tables && length(tables) > 0) {
      new_text <- glue::glue(
        "{new_text}\n\n",
        "These tables are available in the database:\n",
        "  {paste(tables, collapse = ', ')}"
      )
      # Append describe info if available
      if (nzchar(describe_text)) {
        new_text <- glue::glue(
          "{new_text}{describe_text}"
        )
      }
    }

    if (evaluate_code && return_mode == "object") {
      new_text <- glue::glue(
        "{new_text}\n\n",
        "The SQL code should produce a query result that answers the prompt."
      )
    }

    if (output_as_tool) {
      new_text <- glue::glue(
        "{new_text}\n\n",
        "Your SQL query will be executed on the database. The results will be",
        " sent back to you. After seeing the results, you can either provide a",
        " final answer or try another SQL query. When you provide your final answer,",
        " do not include any SQL code."
      )
    }

    return(new_text)
  }

  # Define extraction function to handle returned LLM responses
  extraction_fn <- function(x) {
    return_list <- list()
    return_list$llm_answer <- x

    # Extract SQL code
    extracted_code <- answer_using_sql_extract_sql_code(x)

    if (length(extracted_code) == 0) {
      if (output_as_tool) {
        return(x)
      }

      return(
        llm_feedback(
          paste0(
            "No SQL code detected. You must provide SQL code ",
            "between ```sql and ```."
          )
        )
      )
    }

    # For simplicity, assume one SQL code block; if multiple, use the first
    sql_code <- extracted_code[1]
    return_list$code <- sql_code

    if (!evaluate_code) {
      # Just return the code or final answer
      if (return_mode == "code") return(sql_code)
      if (return_mode == "llm_answer") return(x)
      # 'full' without evaluation just returns code + llm_answer
      if (return_mode == "full") return(return_list)
      return(sql_code)
    }

    # Evaluate the SQL code
    query_res <- tryCatch(
      DBI::dbGetQuery(conn, sql_code),
      error = function(e) e
    )

    if (inherits(query_res, "error")) {
      return(
        llm_feedback(
          glue::glue(
            "An error occurred while executing the SQL:\n",
            "    {query_res$message}\n",
            "Please provide a valid SQL query."
          )
        )
      )
    }

    # query_res should now be a data frame
    return_list$output <- query_res

    formatted_output <- glue::glue(
      "--- SQL code: ---\n",
      "{sql_code}\n\n",
      "--- Query results: ---\n",
      "{utils::capture.output(print(query_res)) |> paste(collapse = \"\\n\")}"
    )
    return_list$formatted_output <- formatted_output

    if (output_as_tool) {
      # Send formatted output back as tool result
      return(llm_feedback(formatted_output, tool_result = TRUE))
    }

    # Return according to return_mode
    if (return_mode == "full") return(return_list)
    if (return_mode == "code") return(sql_code)
    if (return_mode == "console")
      return(utils::capture.output(print(query_res)) |> paste(collapse = "\n"))
    if (return_mode == "object") return(query_res)
    if (return_mode == "formatted_output") return(formatted_output)
    if (return_mode == "llm_answer") return(x)

    return(return_list)
  }

  # If we are sending back output as a tool
  type <- "unspecified"
  if (output_as_tool) {
    type <- "tool"
  }

  # Wrap the prompt
  prompt_wrap(
    prompt,
    modify_fn,
    extraction_fn,
    type = type,
    name = "answer_using_sql"
  )
}

#' Helper function to extract SQL code from a string
#'
#' This function extracts SQL code from a string by matching all content between
#' sql tags.
#'
#' @param input_string A string containing SQL code, typically a response
#' from an LLM
#'
#' @return A character vector containing the extracted SQL code
#'
#' @noRd
#' @keywords internal
answer_using_sql_extract_sql_code <- function(input_string) {
  # Use regular expression to match all content between ```sql and ```, with case-insensitive matching
  matches <- gregexpr(
    "(?s)```[sS][qQ][lL]\\s*(.*?)\\s*```",
    input_string,
    perl = TRUE
  )
  extracted_code <- regmatches(input_string, matches)

  # Remove the ```sql and ``` wrappers
  extracted_code <- lapply(extracted_code, function(x) {
    sub("(?s)```[sS][qQ][lL]\\s*(.*?)\\s*```", "\\1", x, perl = TRUE)
  })

  return(unlist(extracted_code))
}
