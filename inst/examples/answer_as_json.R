base_prompt <- "How can I solve 8x + 7 = -23?"

# This example will show how to enforce JSON format in the response,
#   with and without a schema, using the 'answer_as_json()' prompt wrap.
#   Based on which LLM provider you are using, the JSON format enforcement
#   will automatically be handled in the best way. This also means that
#   you can easily swap between different LLM providers while still
#   achieving the same results

#### Enforcing JSON without a schema: ####

\dontrun{
  # Ollama:
  #   Sets 'format' parameter to 'json', enforcing JSON
  #   Adds request to prompt for a JSON object, as is recommended by the docs
  #   Parses JSON to R object
  json1 <- base_prompt |>
    answer_as_json() |>
    send_prompt(llm_provider_ollama())
  # --- Sending request to LLM provider (llama3.1:8b): ---
  # How can I solve 8x + 7 = -23?
  #
  # Your must format your response as a JSON object.
  # --- Receiving response from LLM provider: ---
  # {"steps": [
  #   "Subtract 7 from both sides to get 8x = -30",
  #   "Simplify the right side of the equation to get 8x = -30",
  #   "Divide both sides by 8 to solve for x, resulting in x = -30/8",
  #   "Simplify the fraction to find the value of x"
  # ],
  # "value_of_x": "-3.75"}

  # OpenAI-type API without schema
  #   Sets 'response_format' parameter to 'json_object', enforcing JSON
  #   Adds request to prompt for a JSON object, as is required by the API
  #   Parses JSON to R object
  json2 <- base_prompt |>
    answer_as_json() |>
    send_prompt(llm_provider_openai())
  # --- Sending request to LLM provider (gpt-4o-mini): ---
  # How can I solve 8x + 7 = -23?
  #
  # Your must format your response as a JSON object.
  # --- Receiving response from LLM provider: ---
  # {
  #   "solution_steps": [
  #     {
  #       "step": 1,
  #       "operation": "Subtract 7 from both sides",
  #       "equation": "8x + 7 - 7 = -23 - 7",
  #       "result": "8x = -30"
  #     },
  #     {
  #       "step": 2,
  #       "operation": "Divide both sides by 8",
  #       "equation": "8x / 8 = -30 / 8",
  #       "result": "x = -3.75"
  #     }
  #   ],
  #   "solution": {
  #     "x": -3.75
  #   }
  # }

  # ... (Creating fake provider based on Ollama for demonstration purposes) ...
  fake_provider_without_json_support <- llm_provider_ollama()
  fake_provider_without_json_support$api_type <- "fake"
  fake_provider_without_json_support$parameters$stream <- FALSE

  # Text-based without schema (works for any provider/model)
  #   Adds request to prompt for a JSON object
  #   Extracts JSON from textual response (feedback for retry if no JSON received)
  #   Parses JSON to R object
  json3 <- base_prompt |>
    answer_as_json() |>
    send_prompt(fake_provider_without_json_support)
  # --- Sending request to LLM provider (llama3.1:8b): ---
  # How can I solve 8x + 7 = -23?
  #
  # Your must format your response as a JSON object.
  # --- Receiving response from LLM provider: ---
  # Here is the solution to the equation formatted as a JSON object:
  #
  # ```
  # {
  #   "equation": "8x + 7 = -23",
  #   "steps": [
  #     {
  #       "step": "Subtract 7 from both sides of the equation",
  #       "expression": "-23 - 7"
  #     },
  #     {
  #       "step": "Simplify the expression on the left side",
  #       "result": "-30"
  #     },
  #     {
  #       "step": "Divide both sides by -8 to solve for x",
  #       "expression": "-30 / -8"
  #     },
  #     {
  #       "step": "Simplify the expression on the right side",
  #       "result": "3.75"
  #     }
  #   ],
  #   "solution": {
  #     "x": 3.75
  #   }
  # }
  # ```
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
# Note: when you are not using an OpenAI API, you can also pass just the
#   internal 'schema' list object to 'answer_as_json()' instead of the full
#   'json_schema' list object

# Generate example R object based on schema:
r_json_schema_to_example(json_schema)

\dontrun{
  # Ollama with schema
  #   Sets 'format' parameter to 'json', enforcing JSON
  #   Adds request to prompt for a JSON object, as is recommended by the docs
  #   Adds schema to prompt
  #   Validates JSON against schema with 'jsonvalidate' package (feedback for retry if invalid)
  json4 <- base_prompt |>
    answer_as_json(json_schema) |>
    send_prompt(llm_provider_ollama())
  # --- Sending request to LLM provider (llama3.1:8b): ---
  # How can I solve 8x + 7 = -23?
  #
  # Your must format your response as a JSON object.
  #
  # Your JSON object should match this example JSON object:
  # {
  #   "steps": [
  #     {
  #       "explanation": "...",
  #       "output": "..."
  #     }
  #   ],
  #   "final_answer": "..."
  # }
  # --- Receiving response from LLM provider: ---
  # {
  #   "steps": [
  #     {
  #       "explanation": "First, subtract 7 from both sides of the equation to
  #       isolate the term with x.",
  #       "output": "8x = -23 - 7"
  #     },
  #     {
  #       "explanation": "Simplify the right-hand side of the equation.",
  #       "output": "8x = -30"
  #     },
  #     {
  #       "explanation": "Next, divide both sides of the equation by 8 to solve for x.",
  #       "output": "x = -30 / 8"
  #     },
  #     {
  #       "explanation": "Simplify the right-hand side of the equation.",
  #       "output": "x = -3.75"
  #     }
  #   ],
  #   "final_answer": "-3.75"
  # }

  # OpenAI with schema
  #   Sets 'response_format' parameter to 'json_object', enforcing JSON
  #   Adds json_schema to the API request, API enforces JSON adhering schema
  #   Parses JSON to R object
  json5 <- base_prompt |>
    answer_as_json(json_schema) |>
    send_prompt(llm_provider_openai())
  # --- Sending request to LLM provider (gpt-4o-mini): ---
  # How can I solve 8x + 7 = -23?
  # --- Receiving response from LLM provider: ---
  # {"steps":[
  # {"explanation":"Start with the original equation.",
  # "output":"8x + 7 = -23"},
  # {"explanation":"Subtract 7 from both sides to isolate the term with x.",
  # "output":"8x + 7 - 7 = -23 - 7"},
  # {"explanation":"Simplify the left side and the right side of the equation.",
  # "output":"8x = -30"},
  # {"explanation":"Now, divide both sides by 8 to solve for x.",
  # "output":"x = -30 / 8"},
  # {"explanation":"Simplify the fraction by dividing both the numerator and the
  # denominator by 2.",
  # "output":"x = -15 / 4"}
  # ], "final_answer":"x = -15/4"}

  # Text-based with schema (works for any provider/model)
  #   Adds request to prompt for a JSON object
  #   Adds schema to prompt
  #   Extracts JSON from textual response (feedback for retry if no JSON received)
  #   Validates JSON against schema with 'jsonvalidate' package (feedback for retry if invalid)
  #   Parses JSON to R object
  json6 <- base_prompt |>
    answer_as_json(schema = json_schema) |>
    send_prompt(fake_provider_without_json_support)
  # --- Sending request to LLM provider (llama3.1:8b): ---
  # How can I solve 8x + 7 = -23?
  #
  # Your must format your response as a JSON object.
  #
  # Your JSON object should match this example JSON object:
  #   {
  #     "steps": [
  #       {
  #         "explanation": "...",
  #         "output": "..."
  #       }
  #     ],
  #     "final_answer": "..."
  #   }
  # --- Receiving response from LLM provider: ---
  # Here is the solution to the equation:
  #
  # ```
  # {
  #   "steps": [
  #     {
  #       "explanation": "First, we want to isolate the term with 'x' by
  #       subtracting 7 from both sides of the equation.",
  #       "output": "8x + 7 - 7 = -23 - 7"
  #     },
  #     {
  #       "explanation": "This simplifies to: 8x = -30",
  #       "output": "8x = -30"
  #     },
  #     {
  #       "explanation": "Next, we want to get rid of the coefficient '8' by
  #       dividing both sides of the equation by 8.",
  #       "output": "(8x) / 8 = (-30) / 8"
  #     },
  #     {
  #       "explanation": "This simplifies to: x = -3.75",
  #       "output": "x = -3.75"
  #     }
  #   ],
  #   "final_answer": "-3.75"
  # }
  # ```
}
