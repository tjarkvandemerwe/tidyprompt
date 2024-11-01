paste0(
  "Using the built-in airquality dataset in R,",
  " calculate the average Ozone level for days in May with temperatures",
  " above 80°F."
) |>
  answer_as_integer() |>
  answer_as_code(pkgs_to_use = c("dplyr")) |>
  answer_by_chain_of_thought() |>
  send_prompt(llm_provider_openai())


paste0(
  "Using the built-in mtcars dataset, calculate the average miles per gallon (mpg) for cars with 6 cylinders."
) |>
  answer_as_integer() |>
  answer_as_code(pkgs_to_use = c("dplyr")) |>
  answer_by_chain_of_thought() |>
  send_prompt()

plot_code <- glue::glue(
  "Using ggplot2, create a scatter plot of miles per gallon (mpg) versus horsepower (hp) for the cars in the mtcars dataset. Use different colors to represent the number of cylinders (cyl)."
) |> as.character() |>
  answer_as_code(
    pkgs_to_use = c("ggplot2"), evaluate_code = FALSE, send_back_output = FALSE
  ) |>
  answer_by_chain_of_thought() |>
  send_prompt()
