test_that("can set sytem prompt", {
  prompt <- "hi" |>
    set_system_prompt("you only say 'beep boop'")

  expect_equal(prompt$system_prompt, "you only say 'beep boop'")

  chat_history <- prompt$get_chat_history()
  expect_equal(chat_history$role[1], "system")
  expect_equal(chat_history$content[1], "you only say 'beep boop'")
})
