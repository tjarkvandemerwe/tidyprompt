test_that("answer_as_boolean works correctly", {
  skip_test_if_no_ollama()

  response <- "Is the sky blue?" |>
    answer_as_boolean() |>
    send_prompt(llm_provider_ollama(), verbose = FALSE)

  expect_true(is.logical(response))
  expect_true(response)
})

test_that("answer_as_boolean handles true_definition and false_definition", {
  prompt <- "Is water wet?" |>
    answer_as_boolean(
      true_definition = "Water is wet",
      false_definition = "Water is not wet"
    ) |> construct_prompt_text()

  expect_true(grepl("TRUE means: Water is wet", prompt))
  expect_true(grepl("FALSE means: Water is not wet", prompt))
})
