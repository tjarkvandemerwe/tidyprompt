test_that("answer_as_list adds list instruction", {
  prompt <- "List the top programming languages." |>
    answer_as_list(item_name = "language", n_unique_items = 3, list_mode = "bullet") |>
    construct_prompt_text()

  expect_true(
    grepl(
      "Respond with a list, like so:\n  -- <<language 1>>\n  -- <<language 2>>\n  etc.",
      prompt,
      fixed = TRUE
    )
  )
  expect_true(
    grepl(
      "The list should contain 3 unique items.",
      prompt,
      fixed = TRUE
    )
  )
})

test_that("answer_as_list returns list of items", {
  skip_test_if_no_ollama()

  ollama <- llm_provider_ollama()

  response <- "List the primary colors." |>
    answer_as_list(item_name = "color", list_mode = "comma") |>
    send_prompt(ollama, verbose = FALSE)

  expect_true(is.list(response))
  expect_equal(length(response), 3)
  expect_true(all(c("red", "blue", "yellow") %in% tolower(unlist(response))))
})
