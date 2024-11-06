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

paste0(
  "Create a scatter plot of miles per gallon (mpg) versus",
  " horsepower (hp) for the cars in the mtcars dataset.",
  " Use different colors to represent the number of cylinders (cyl).",
  " Be very creative and make the plot look nice but also a little crazy!"
) |>
  answer_as_code(
    pkgs_to_use = c("ggplot2"),
    evaluate_code = TRUE,
    return_mode = "object"
  ) |>
  send_prompt(anthropic)


###

rs <- callr::r_session$new()
rs$run_with_output(function() {
  # Add labels to each column
  attr(cars$speed, "label") <- "Humidity in Overijssel (%)"
  attr(cars$dist, "label") <- "Number of bananas sold in Enschede"

  # rename
  names(cars) <- c("Humidity", "Bananas")

  dataset <<- cars;

  head(dataset)
})

answer <- "Please do an analysis on the dataset. Think of 1 interesting
research question and answer it." |>
  answer_as_code(
    pkgs = c("ggplot2", "dplyr"),
    evaluation_session = rs,
    output_as_tool = TRUE
  ) |>
  answer_by_chain_of_thought(extract_from_finish_brackets = FALSE) |>
  send_prompt()


answer <- "What do you think of this dataset?" |>
  answer_as_code(
    pkgs = c("ggplot2", "dplyr"),
    evaluation_session = rs,
    output_as_tool = TRUE
  ) |>
  answer_by_chain_of_thought(extract_from_finish_brackets = TRUE) |>
  send_prompt(anthropic)


plot <- "Please do an analysis on the dataset. Think of 1 interesting
research question and answer it with a nice plot. Make your plot a little crazy!" |>
  answer_as_code(
    pkgs = c("ggplot2", "dplyr"),
    evaluation_session = rs,
    return_mode = "object"
  ) |>
  answer_by_chain_of_thought(extract_from_finish_brackets = FALSE) |>
  send_prompt(anthropic)
