test_that("can create prompt_wrap", {
  prompt <- prompt_wrap("aaa", modify_fn = \(x) "bbb")
  expect_s3_class(prompt, "prompt")
})
