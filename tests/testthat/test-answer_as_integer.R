

test_that("answer_as_int returns int", {
  skip_test_if_no_ollama()

  ollama <- llm_provider_ollama()

  response <- "How much is 2+2?" |>
    answer_as_integer() |>
    send_prompt(ollama, verbose = FALSE)

  expect_true(is.numeric(response))

})
