test_that("answer_as_key_value returns key-value list", {
  skip_test_if_no_ollama()

  ollama <- llm_provider_ollama()

  response <- "Provide a list of key-value pairs." |>
    answer_as_key_value() |>
    send_prompt(ollama, verbose = FALSE)

  expect_true(is.list(response))
  expect_true(all(sapply(names(response), is.character)))
  expect_true(all(sapply(response, is.character)))
})
