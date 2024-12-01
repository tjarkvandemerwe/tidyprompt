base_prompt <- "How can I solve 8x + 7 = -23?"

#### Enforcing JSON without a schema: ####

\dontrun{
  # Text-based (works with any model and all providers):
  #   Adds request for JSON object to the prompt
  #   Extracts the JSON objects from the LLM response
  json1 <- base_prompt |>
    answer_as_json() |>
    send_prompt(llm_provider_ollama())

  # Ollama-type without schema
  #   Sets Ollama's 'format' parameter to 'json', enforcing JSON
  #   Adds request to prompt for a JSON object, as is recommended in documentation
  json2 <- base_prompt |>
    answer_as_json(type = "ollama") |>
    send_prompt(llm_provider_ollama())

  # OpenAI-type without schema
  #   Sets OpenAI's 'response_format' parameter to 'json_object', enforcing JSON
  #   Adds request to prompt for a JSON object, as is required by the API
  json3 <- base_prompt |>
    answer_as_json(type = "openai") |>
    send_prompt(llm_provider_openai())
}



#### Enforcing JSON with a schema: ####

# Make a list representing a JSON schema,
#   which the LLM response must adhere to:
json_schema <- list(
  name = "steps_to_solve", # Required for OpenAI API
  description = NULL, # Optional for OpenAI API
  schema = list(
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
  # 'strict' parameter is set as argument 'answer_as_json()'
)

# Generate example R object based on schema:
generate_json_example_from_schema(json_schema)

\dontrun{
  # Text-based with schema
  #   Adds request for JSON & example based on schema to the prompt;
  #   extracts JSON and validates against the schema with
  #   the 'jsonvalidate' package
  json4 <- base_prompt |>
    answer_as_json(schema = json_schema) |>
    send_prompt(llm_provider_ollama())

  # Ollama with schema
  #   Sets Ollama's 'format' parameter to 'json', enforcing JSON
  #   Adds example based on schema to the prompt;
  #   extracts JSON and validates against the schema with
  #   the 'jsonvalidate' package
  json5 <- base_prompt |>
    answer_as_json(type = "ollama", schema = json_schema) |>
    send_prompt(llm_provider_ollama())

  # OpenAI with schema
  #   Sets OpenAI's 'response_format' parameter to 'json_schema',
  #   and adds the json_schema to the API request;
  #   as such, the API will natively enforce the schema
  json6 <- base_prompt |>
    answer_as_json(type = "openai", schema = json_schema) |>
    send_prompt(llm_provider_openai())
}
