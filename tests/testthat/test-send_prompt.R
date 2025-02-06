test_that("extraction and validation works", {
  fake_llm <- llm_provider_fake()

  response <- "What is 2 + 2?" |>
    answer_by_chain_of_thought() |>
    answer_as_integer() |>
    send_prompt(fake_llm, verbose = TRUE)

  is_whole_number <- function(x) {
    is.numeric(x) && x == floor(x)
  }

  expect_true(is_whole_number(response))
  expect_equal(response, 4)
})

test_that("full return mode works", {
  fake_llm <- llm_provider_fake()

  response <- "hi" |> send_prompt(fake_llm, return_mode = "full")

  expect_type(response$response, "character")
  expect_type(response$interactions, "double")

  expect_true(length(response$response) == 1)
  expect_true(is.data.frame(response$chat_history))
  expect_true(is.data.frame(response$chat_history_clean))
  expect_true(
    is.numeric(response$interactions) &
      response$interactions > 0 &
      response$interactions == floor(response$interactions)
  )
  expect_true(is.double(response$duration_seconds))
})
