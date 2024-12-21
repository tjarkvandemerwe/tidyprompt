\dontrun{
  # Prompt to value calculated with R
  avg_miles_per_gallon <- paste0(
    "Using my data,",
    " calculate the average miles per gallon (mpg) for cars with 6 cylinders."
  ) |>
    answer_as_integer() |>
    answer_using_r(
      pkgs_to_use = c("dplyr"),
      objects_to_use = list(mtcars = mtcars),
      evaluate_code = TRUE,
      output_as_tool = TRUE
    ) |>
    send_prompt()
  avg_miles_per_gallon

  # Prompt to linear model object in R
  model <- paste0(
    "Using my data, create a statistical model",
    " investigating the relationship between two variables."
  ) |>
    answer_using_r(
      objects_to_use = list(data = mtcars),
      evaluate_code = TRUE,
      return_mode = "object"
    ) |>
    prompt_wrap(
      validation_fn = function(x) {
        if (!inherits(x, "lm"))
          return(llm_feedback("The output should be a linear model object."))
        return(x)
      }
    ) |>
    send_prompt()
  summary(model)

  # Prompt to plot object in R
  plot <- paste0(
    "Create a scatter plot of miles per gallon (mpg) versus",
    " horsepower (hp) for the cars in my data.",
    " Use different colors to represent the number of cylinders (cyl).",
    " Be very creative and make the plot look nice but also a little crazy!"
  ) |>
    answer_using_r(
      pkgs_to_use = c("ggplot2"),
      objects_to_use = list(mtcars = mtcars),
      evaluate_code = TRUE,
      return_mode = "object"
    ) |>
    send_prompt()
  plot
}
