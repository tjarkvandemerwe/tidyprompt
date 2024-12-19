test_that("answer_as_json returns valid JSON", {
  skip_test_if_no_ollama()

  ollama <- llm_provider_ollama()

  response <- "Provide a JSON object with name and age." |>
    answer_as_json() |>
    send_prompt(ollama, verbose = FALSE)

  expect_true(jsonlite::validate(response))
})
