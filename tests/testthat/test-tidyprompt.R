test_that("create prompt", {
  prompt <- "Hi" |> tidyprompt()
  expect_s3_class(prompt, "Tidyprompt")
})

test_that("construct simple prompt text", {
  prompt <- "Hi" |> tidyprompt()
  expect_equal(construct_prompt_text(prompt), "Hi")
})

test_that("construct more complex prompt text", {
  prompt <- "Hi" |> add_text("How are you?", sep = "\n\n", position = "after")
  expect_equal(
    construct_prompt_text(prompt), "Hi\n\nHow are you?"
  )
})
