paste0(
  "Using the built-in airquality dataset in R,",
  " calculate the average Ozone level for days in May with temperatures",
  " above 80°F."
) |>
  answer_as_integer() |>
  answer_as_code(pkgs_to_use = c("dplyr")) |>
  answer_by_chain_of_thought() |>
  send_prompt(llm_provider_openai())
