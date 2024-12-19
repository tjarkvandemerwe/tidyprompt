test_that("answer using r can create linear model with data", {
  skip_test_if_no_ollama()

  # Prompt to linear model object in R
  model <- paste0(
    "Using my data, create a statistical model",
    " investigating the relationship between two variables."
  ) |>
    answer_using_r(
      objects_to_use = list(data = mtcars),
      evaluate_code = TRUE,
      return_mode = "object"
    ) |>
    prompt_wrap(
      validation_fn = function(x) {
        if (!inherits(x, "lm"))
          return(llm_feedback("The output should be a linear model object."))
        return(x)
      }
    ) |>
    send_prompt(llm_provider_ollama(), verbose = FALSE)

  # Check if the model is a linear model object
  expect_true(inherits(model, "lm"))
})
