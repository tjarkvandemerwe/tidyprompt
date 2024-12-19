test_that("answer_as_list returns list", {
  skip_test_if_no_ollama()

  ollama <- llm_provider_ollama()

  response <- "Provide a list of items." |>
    answer_as_list() |>
    send_prompt(ollama, verbose = FALSE)

  expect_true(is.list(response))
})
