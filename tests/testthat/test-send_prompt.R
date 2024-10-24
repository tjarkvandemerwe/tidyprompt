test_that("extraction and validation works", {
 ollama <- create_fake_llm_provider()

  response <- "What is 2 + 2?" |>
    answer_by_chain_of_thought() |>
    answer_as_integer() |>
    send_prompt(ollama, verbose = TRUE)

  expect_true(is.integer(response))
  expect_equal(response, 4)
})
