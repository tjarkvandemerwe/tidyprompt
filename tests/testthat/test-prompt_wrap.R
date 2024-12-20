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

test_that("handler fn works", {
  fake <- llm_provider_fake()

  result <- "Hi" |>
    prompt_wrap(
      handler_fn = function(response, llm_provider) {
        if (!inherits(llm_provider, "LlmProvider")) {
          stop("llm_provider passed by handler_fn is not an LlmProvider")
        }

        response$completed$content[nrow(response$completed)] <- "beepido boop ba"
        return(response)
      }
    ) |>
    send_prompt(fake)

  expect_identical(result, "beepido boop ba")
})

test_that("parameter_fn throws no error", {
  fake <- llm_provider_fake()

  # Expect no error
  expect_no_error("Hi" |>
    prompt_wrap(
      parameter_fn = function(llm_provider) {
        if (!inherits(llm_provider, "LlmProvider")) {
          stop("llm_provider passed by parameter_fn is not an LlmProvider")
        }

        return(list(
          beep = "boop"
        ))
      }
    ) |>
    send_prompt(fake)
  )
})
