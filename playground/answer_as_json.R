create_math_reasoning_schema <- function() {
  list(
    name = "",
    list(
      type = "object",
      properties = list(
        steps = list(
          type = "array",
          items = list(
            type = "object",
            properties = list(
              explanation = list(type = "string"),
              output = list(type = "string")
            ),
            required = c("explanation", "output"),
            additionalProperties = FALSE
          )
        ),
        final_answer = list(type = "string")
      ),
      required = c("steps", "final_answer"),
      additionalProperties = FALSE
    )
  )
}

bad_schema <- list(
  aaa = "dfsafdsa"
)

# Example of using the schema with `answer_as_json`
schema <- create_math_reasoning_schema()

answer_as_json <- function(
    prompt,
    schema = NULL,
    type = c("openai_api", "text-based")
) {
  prompt <- tidyprompt(prompt)
  type <- match.arg(type)

  if (type == "openai_api") {
    if (is.null(schema)) {
      prompt$parameters$response_format <- list(type = "json_object")
    } else {
      prompt$paramters$response_format <- list(
        type = "json_schema",
        json_schema = schema
      )
    }
  }

  modify_fn <- function(original_prompt_text) {
    glue::glue(
      "{original_prompt_text}\n\n",
      "Respond with a aaa object."
    )
  }

  extraction_fn <- function(llm_response) {
    jsons <- extraction_fn_json(llm_response)
    if (length(jsons) == 1) jsons <- jsons[[1]]
    jsons
  }

  prompt_wrap(prompt, modify_fn, extraction_fn)
}

json <- "How can I solve 8x + 7 = -23?" |>
  answer_as_json(schema = NULL) |>
  send_prompt(llm_provider_openai(), stream = FALSE)




