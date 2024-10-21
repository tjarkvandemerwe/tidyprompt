test_that("add text to prompt", {
  prompt <- "Prompt" |>
    add_text("Text after") |>
    add_text("Text before", position = "before")

  constructed_text <- prompt |> construct_prompt_text()

  expect_equal(
    constructed_text,
    "Text before\n\nPrompt\n\nText after"
  )
})
