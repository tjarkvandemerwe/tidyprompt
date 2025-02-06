test_that("answer_as_key_value adds key-value pair instruction", {
  prompt <- "List the capitals of countries." |>
    answer_as_key_value(
      key_name = "country",
      value_name = "capital",
      n_unique_items = 2,
      list_mode = "bullet"
    ) |>
    construct_prompt_text()

  expect_true(
    grepl(
      paste0(
        "Respond with a list of key-value pairs, like so:\n  ",
        "-- <<country 1>>: <<capital 1>>\n  ",
        "-- <<country 2>>: <<capital 2>>"
      ),
      prompt,
      fixed = TRUE
    )
  )
  expect_true(
    grepl(
      "The list should contain 2 unique pairs.",
      prompt,
      fixed = TRUE
    )
  )
})

test_that("answer_as_key_value returns key-value pairs", {
  skip_test_if_no_ollama()

  ollama <- llm_provider_ollama()

  response <- "Provide the capitals of these countries: The Netherlands, France, Japan." |>
    answer_as_key_value(
      key_name = "country",
      value_name = "capital",
      list_mode = "comma"
    ) |>
    send_prompt(ollama)

  expect_true(is.list(response))
  expect_equal(length(response), 3)
  expect_true(all(c("The Netherlands", "France", "Japan") %in% names(response)))
  expect_true(all(c("Amsterdam", "Paris", "Tokyo") %in% response))
})
