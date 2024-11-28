"Hi! Create a persona for me please." |>
  answer_as_named_list(item_names = c(
    "name", "age", "city"
  )) |>
  send_prompt(llm_provider_openai())

# Will succeed thanks to item instruction
"Hi! Create a persona for me please." |>
  answer_as_named_list(
    item_names = c(
    "name", "age", "city"
    ),
    item_instructions = list(
      "age" = "Must be written as a whole word (not a number)"
    ),
    item_validations = list(
      "age" = function(x) {
        # Verify that age is *not* numeric
        numeric <- suppressWarnings(as.numeric(x))
        if (!is.na(numeric)) {
          return(create_llm_feedback(
            "Please write age as a whole word (not a number)"
          ))
        }
      }
    )
  ) |>
  send_prompt(llm_provider_openai())

# Will fail initially without item instruction, but will pass after validation feedback
"Hi! Create a persona for me please." |>
  answer_as_named_list(
    item_names = c(
      "name", "age", "city"
    ),
    item_validations = list(
      "age" = function(x) {
        # Verify that age is *not* numeric
        numeric <- suppressWarnings(as.numeric(x))
        if (!is.na(numeric)) {
          return(create_llm_feedback(
            "Please write age as a whole word (not a number)"
          ))
        }
      }
    )
  ) |>
  send_prompt(llm_provider_openai())
