paste0(
  "Using the built-in airquality dataset in R,",
  " calculate the average Ozone level for days in May with temperatures",
  " above 80°F."
) |>
  answer_as_integer() |>
  answer_as_code(
    pkgs_to_use = c("dplyr"), output_as_tool = TRUE, return_mode = "llm_answer"
  ) |>
  answer_by_chain_of_thought() |>
  send_prompt(llm_provider_openai())

paste0(
  "Using the built-in mtcars dataset, calculate the average miles per gallon (mpg) for cars with 6 cylinders."
) |>
  answer_as_integer() |>
  answer_as_code(
    pkgs_to_use = c("dplyr"), output_as_tool = TRUE
  ) |>
  answer_by_chain_of_thought() |>
  send_prompt()

plot <- paste0(
  "Create a scatter plot of miles per gallon (mpg) versus",
  " horsepower (hp) for the cars in the mtcars dataset.",
  " Use different colors to represent the number of cylinders (cyl)."
) |>
  answer_as_code(
    pkgs_to_use = c("ggplot2"),
    evaluate_code = TRUE,
    return_mode = "object"
  ) |>
  send_prompt()
plot

anthropic <- llm_provider_openrouter()$
  set_parameters(list(model = "anthropic/claude-3.5-sonnet"))
plot <- paste0(
  "Create a scatter plot of miles per gallon (mpg) versus",
  " horsepower (hp) for the cars in the mtcars dataset.",
  " Use different colors to represent the number of cylinders (cyl).",
  " Be very creative and make the plot look nice but also a little crazy!",
  " Add some humourous annotations which somehow relate to Enschede (NL)"
) |>
  answer_as_code(
    pkgs_to_use = c("ggplot2"),
    evaluate_code = TRUE,
    return_mode = "object"
  ) |>
  send_prompt(anthropic)
plot
