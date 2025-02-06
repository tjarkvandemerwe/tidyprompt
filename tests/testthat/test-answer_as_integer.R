test_that("answer_as_int returns int", {
  skip_test_if_no_ollama()

  ollama <- llm_provider_ollama()

  response <- "How much is 2+2?" |>
    answer_as_integer() |>
    send_prompt(ollama, verbose = FALSE)

  expect_true(is.numeric(response))
})

test_that("answer_as_integer adds instruction", {
  prompt <- "What is 5+5?" |>
    answer_as_integer() |>
    construct_prompt_text()

  expect_true(
    grepl(
      "You must answer with only an integer (use no other characters).",
      prompt,
      fixed = TRUE
    )
  )
})
