#' @title Make LLM answer as JSON (with optional schema)
#'
#' @description This functions wraps a prompt with settings that ensure the LLM response
#' is a valid JSON object, optionally matching a given JSON schema.
#'
#' The function can work with all models and providers through text-based
#' handling, but also supports native settings for the OpenAI and Ollama
#' API types. (See argument 'type'.) This means that it is possible to easily
#' switch between providers with different levels of JSON support,
#' while ensuring the results will be in the correct format.
#'
#' @param prompt A single string or a [tidyprompt()] object
#'
#' @param schema A list which represents
#' a JSON schema that the response should match.See example and your API's
#' documentation for more information on defining JSON schemas. Note that the schema should be a
#' list (R object) representing a JSON schema, not a JSON string
#' (use [jsonlite::fromJSON()] and [jsonlite::toJSON()] to convert between the two)
#'
#' @param schema_strict If TRUE, the provided schema will be strictly enforced.
#' This option is passed as part of the schema when using type  type
#' "openai" or "ollama", and when using the other types it is passed to
#' [jsonvalidate::json_validate()]
#'
#' @param schema_in_prompt_as If providing a schema and
#' when using type "text-based", "openai_oo", or "ollama_oo", this argument specifies
#' how the schema should be included in the prompt:
#' \itemize{
#' \item "example" (default): The schema will be included as an example JSON object
#' (tends to work best). [r_json_schema_to_example()] is used to generate the example object
#' from the schema
#' \item "schema": The schema will be included as a JSON schema
#' }
#' @param type The way that JSON response should be enforced:
#' \itemize{
#' \item "text-based": Instruction will be added to the prompt
#' asking for JSON; when a schema is provided, this will also be included
#' in the prompt (see argument 'schema_in_prompt_as'). JSON will be parsed
#' from the LLM response and, when a schema is provided, it will be validated
#' against the schema with [jsonvalidate::json_validate()]. Feedback is sent to the
#' LLM when the response is not valid. This option always works, but may in some
#' cases may be less powerful than the other native JSON options
#' \item "auto": Automatically determine the type based on 'llm_provider$api_type'.
#' This does not consider model compatibility and could lead to errors; set 'type'
#' manually if errors occur; use 'text-based' if unsure
#' \item "openai" and "ollama": The response format will be set via the relevant API parameters,
#' making the API enforce a valid JSON response. If a schema is provided,
#' it will also be included in the API parameters and also be enforced by the API.
#' When no schema is provided, a request for JSON is added to the prompt (as required
#' by the APIs). Note that these JSON options may not be available for all models
#' of your provider; consult their documentation for more information.
#' If you are unsure or encounter errors, use "text-based"
#' \item "openai_oo" and "ollama_oo": Similar to "openai" and "ollama", but if a
#' schema is provided it is not included in the API parameters. Schema validation
#' will be done in R with [jsonvalidate::json_validate()]. This can be useful if
#' you want to use the API's JSON support, but their schema support is limited
#' }
#' Note that the "openai" and "ollama" types may also work for other APIs with a similar structure
#'
#' @return A [tidyprompt()] with an added [prompt_wrap()] which will ensure
#' that the LLM response is a valid JSON object
#'
#' @export
#'
#' @example inst/examples/answer_as_json.R
#'
#' @family pre_built_prompt_wraps
#' @family answer_as_prompt_wraps
#' @family json
answer_as_json <- function(
    prompt,
    schema = NULL,
    schema_strict = FALSE,
    schema_in_prompt_as = c(
      "example", "schema"
    ),
    type = c(
      "text-based", "auto", "openai", "ollama", "openai_oo", "ollama_oo"
    )
) {
  prompt <- tidyprompt(prompt)
  schema_in_prompt_as <- match.arg(schema_in_prompt_as)
  type <- match.arg(type)

  if (!is.null(schema) & !is.list(schema))
    stop("The 'schema' argument must be a list (R object) representing a JSON schema")
  if (!is.null(schema) & length(schema) == 0)
    stop("The 'schema' argument must be a non-empty list (R object) representing a JSON schema")

  if (type == "auto" & getOption("tidyprompt.warn.auto.json", TRUE)) {
    cli::cli_alert_warning(paste0(
      "{.strong `answer_as_json()`}:\n",
      "* Automatically determining type based on 'llm_provider$api_type';\n",
      "this does not consider model compatability\n",
      "* Manually set argument 'type' if errors occur ",
      "(\"text-based\" always works)\n",
      "* Use `options(tidyprompt.warn.auto.json = FALSE)` to suppress this warning"
    ))
  }

  determine_type <- function(llm_provider = NULL) {
    if (type != "auto")
      return(type)
    if (isTRUE(llm_provider$api_type == "openai"))
      return("openai")
    if (isTRUE(llm_provider$api_type == "ollama"))
      return("ollama")
    return("text-based")
  }

  parameter_fn <- function(llm_provider) {
    type <- determine_type(llm_provider)

    if (type == "ollama") {
      if (is.null(schema))
        return(list(format = "json"))

      if ("schema" %in% names(schema)) {
        schema <- schema$schema
      }

      schema$strict <- schema_strict

      return(list(format = schema))
    }

    if (type == "openai") {
      if (is.null(schema))
        return(list(response_format = list(type = "json_object")))

      json_schema <- list() # Top level schema
      # Should contain:
      # - name
      # - description (optional)
      # - schema
      # - strict

      if (all("name" %in% names(schema), "schema" %in% names(schema))) {
        json_schema <- schema
      } else {
        json_schema$name <- "schema"
        json_schema$schema <- schema
      }

      json_schema$strict <- schema_strict

      return(list(response_format = list(
        type = "json_schema",
        json_schema = json_schema
      )))
    }

    if (type == "ollama_oo") {
      return(list(format = "json"))
    }

    if (type == "openai_oo") {
      return(list(response_format = list(type = "json_object")))
    }

    return(NULL)
  }

  modify_fn <- function(prompt_text, llm_provider) {
    type <- determine_type(llm_provider)
    if (isTRUE(type %in% c("openai", "ollama")) & !is.null(schema))
      return(prompt_text)

    prompt_text <- glue::glue(
      "{prompt_text}\n\n",
      "Your must format your response as a JSON object."
    )

    if (
      !is.null(schema)
    ) {
      jsonvalidate_installed()

      schema_instruction <- NULL
      if (schema_in_prompt_as == "example") {
        schema_instruction <- paste0(
          "Your JSON object should match this example JSON object:\n",
          jsonlite::toJSON(r_json_schema_to_example(schema), auto_unbox = TRUE, pretty = TRUE)
        )
      } else if (schema_in_prompt_as == "schema") {
        schema_instruction <- paste0(
          "Your JSON object should match this JSON schema:\n",
          jsonlite::toJSON(schema, auto_unbox = TRUE, pretty = TRUE)
        )
      }

      schema_instruction <<- schema_instruction
      prompt_text <- paste0(prompt_text, "\n\n", schema_instruction)
    }

    return(prompt_text)
  }

  extraction_fn <- function(llm_response, llm_provider) {
    type <- determine_type(llm_provider)

    jsons <- extraction_fn_json(llm_response)

    if (length(jsons) == 0)
      return(llm_feedback(
        "You must respond as a valid JSON object."
      ))

    if (length(jsons) == 1)
      jsons <- jsons[[1]]

    if (
      !is.null(schema)
      & !isTRUE(type %in% c("openai", "ollama"))
      & jsonvalidate_installed()
    ) {
      # Convert to JSON
      answer_json <- jsonlite::toJSON(jsons, auto_unbox = TRUE, pretty = TRUE)
      schema_json <- jsonlite::toJSON(schema, auto_unbox = TRUE, pretty = TRUE)

      # Validate JSON with verbose error reporting
      validation_result <- jsonvalidate::json_validate(
        answer_json, schema_json, strict = schema_strict, verbose = TRUE
      )

      if (!validation_result) {
        # Extract error details
        error_details <- attr(validation_result, "errors")

        return(llm_feedback(paste0(
          "Your response did not match the expected JSON schema.\n\n",
          df_to_string(error_details), "\n\n",
          schema_instruction
        )))
      }
    }

    jsons
  }

  prompt_wrap(
    prompt,
    modify_fn, extraction_fn, NULL, NULL, parameter_fn,
    name = "answer_as_json"
  )
}



jsonvalidate_installed <- function() {
  if (!requireNamespace("jsonvalidate", quietly = TRUE)) {
    cli::cli_alert_warning(paste0(
      "{.strong `answer_as_json()`}:\n",
      "* When using type \"text-based\" and providing a schema,\n",
      " the 'jsonvalidate' package must be installed to validate the response\n",
      " against the schema\n",
      "* The 'jsonvalidate' package is not installed;\n",
      " the LLM response will not be validated against the schema"
    ))

    return(invisible(FALSE))
  }
  return(invisible(TRUE))
}
