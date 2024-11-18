# First add some labels to 'mtcars':
mtcars$car <- rownames(mtcars)
mtcars$car <- factor(mtcars$car, levels = rownames(mtcars))
attr(mtcars$car, "label") <- "Name of the car"

# Then skim the data:
mtcars |>
  skim_with_labels_and_levels()
