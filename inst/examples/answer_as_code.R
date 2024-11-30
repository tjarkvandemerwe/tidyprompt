\dontrun{
  # Prompt to plot object in R
  plot <- paste0(
    "Create a scatter plot of miles per gallon (mpg) versus",
    " horsepower (hp) for the cars in the mtcars dataset.",
    " Use different colors to represent the number of cylinders (cyl).",
    " Be very creative and make the plot look nice but also a little crazy!"
  ) |>
    answer_as_code(
      pkgs_to_use = c("ggplot2"),
      objects_to_use = list("mtcars" = mtcars),
      evaluate_code = TRUE,
      return_mode = "object"
    ) |>
    send_prompt(llm_provider_openai())
  plot

  # Prompt to value calculated with R
  avg_miles_per_gallon <- paste0(
    "Using the built-in mtcars dataset,",
    " calculate the average miles per gallon (mpg) for cars with 6 cylinders."
  ) |>
    answer_as_integer() |>
    answer_as_code(
      pkgs_to_use = c("dplyr"), output_as_tool = TRUE
    ) |>
    answer_by_chain_of_thought() |>
    send_prompt(llm_provider_ollama())
  avg_miles_per_gallon
}
