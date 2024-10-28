test_that("extraction and validation works", {
 fake_llm <- llm_provider_fake()

  response <- "What is 2 + 2?" |>
    answer_by_chain_of_thought() |>
    answer_as_integer() |>
    send_prompt(fake_llm, verbose = TRUE)

  expect_true(is.integer(response))
  expect_equal(response, 4)
})
