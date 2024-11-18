\dontrun{
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
}
