test_that("can create prompt_wrap", {
  prompt <- prompt_wrap("aaa", modify_fn = \(x) "bbb")
  expect_s3_class(prompt, "Tidyprompt")
})

test_that("can add prompt wraps of all types and they are ordered correctly", {
  prompt <- "Test prompt" |>
    prompt_wrap(extraction_fn = function(x) x, type = "unspecified") |>
    prompt_wrap(extraction_fn = function(x) x, type = "mode") |>
    prompt_wrap(modify_fn = function(x) x, type = "tool") |>
    prompt_wrap(modify_fn = function(x) x, type = "break") |>
    prompt_wrap(validation_fn = function(x) x, type = "check")

  wraps_mod <- prompt$get_prompt_wraps(order = "modification")
  types_in_order_mod <- sapply(wraps_mod, function(x) x$type)
  expect_equal(types_in_order_mod, c("check", "unspecified", "break", "mode", "tool"))

  wraps_eval <- prompt$get_prompt_wraps(order = "evaluation")
  types_in_order_eval <- sapply(wraps_eval, function(x) x$type)
  expect_equal(types_in_order_eval, c("tool", "mode", "break", "unspecified", "check"))
})
