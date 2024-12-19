test_that("answer_as_boolean returns boolean", {
  skip_test_if_no_ollama()

  ollama <- llm_provider_ollama()

  response <- "Is the sky blue?" |>
    answer_as_boolean() |>
    send_prompt(ollama, verbose = FALSE)

  expect_true(is.logical(response))
})
