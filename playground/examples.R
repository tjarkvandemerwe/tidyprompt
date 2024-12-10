library(tidyprompt)

#### 1 LLM provider ####

# Ollama
ollama <- llm_provider_ollama()

# OpenAI
oai <- llm_provider_openai()

# Claude
or <- llm_provider_openrouter()
or$parameters$model <- "anthropic/claude-3.5-haiku"



#### 2 Basic prompt ####

oai$complete_chat("Hi!! How are you?")
oai$complete_chat("What is 2+2?")



#### 3 Basic prompt wraps ####

"What is 2+2" |>
  answer_as_integer() |>
  send_prompt(oai)

# Na feedback toch het correcte format:
"What is 2+2?" |>
  answer_as_integer(add_instruction_to_prompt = FALSE) |>
  send_prompt(oai)


## Verschillende formats mogelijk

# Boolean
"Are you a large language model?" |>
  answer_as_boolean() |>
  send_prompt(oai)
#> [1] TRUE

# Regex
"What animal is the biggest?" |>
  answer_as_regex("^(cat|dog|elephant)$") |>
  send_prompt(oai)
#> [1] "elephant"

# Named list
persona <- "Create 1 persona of an adult for me" |>
  answer_as_named_list(
    c("name", "age", "occupation")
  ) |>
  prompt_wrap(
    extraction_fn = function(persona) {
      # Turn age into a number; if not a number, return feedback
      persona$age <- as.numeric(persona$age)
      if (is.na(persona$age))
        return(llm_feedback("The age should be a number."))

      return(persona)
    },

    validation_fn = function(persona) {
      # Validate that the age is 18 or older
      if (persona$age < 18)
        return(llm_feedback("The age should be 18 or older."))

      return(TRUE)
    }
  ) |>
  send_prompt(oai)
persona$name
persona$age
persona$occupation



#### 4 Reasoning modes ####

"What is 2+2?" |>
  answer_as_integer() |>
  answer_by_chain_of_thought() |>
  send_prompt(oai)



#### 5 Escape prompt ####

# Als vraag niet beantwoord kan worden, ontsnappen aan validatie
"Hoeveel jaar oud is mijn kat Rudolf gisteren geworden?" |>
  answer_as_integer() |>
  quit_if() |>
  send_prompt(oai)
#> NULL



#### 6 R code generation + evaluation ####

# Calculate something in R
result <- "What is 21433214+24332423?" |>
  answer_using_r(
    evaluate_code = TRUE,
    return_mode = "full"
  ) |>
  send_prompt(oai)
result$formatted_output

# Use R code as a tool to answer a question about data
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
  send_prompt(llm_provider_ollama())
avg_miles_per_gallon

# From prompt + data to linear model object in R
model <- paste0(
  "Using my data, create a statistical model",
  " investigating the relationship between two variables."
) |>
  answer_using_r(
    objects_to_use = list(data = cars),
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
  send_prompt(oai)
summary(model)

# Prompt to plot object in R
plot <- paste0(
  "Create a scatter plot of miles per gallon (mpg) versus",
  " horsepower (hp) for the cars in my data.",
  " Use different colors to represent the number of cylinders (cyl).",
  " Be very creative and make the plot look nice but also a little crazy!",
  " Include some jokes about Enschede in the plot"
) |>
  answer_using_r(
    pkgs_to_use = c("ggplot2"),
    objects_to_use = list(mtcars = mtcars),
    evaluate_code = TRUE,
    return_mode = "object"
  ) |>
  send_prompt(or)
plot




#### 7 SQL database interaction ####

# Create an in-memory SQLite database
conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

# Create a sample table of customers
DBI::dbExecute(conn, "
  CREATE TABLE
    customers (
      id INTEGER PRIMARY KEY,
      name TEXT,
      email TEXT,
      country TEXT
    );
")

# Insert some sample customer data
DBI::dbExecute(conn, "
  INSERT INTO
    customers (name, email, country)
  VALUES
    ('Alice', 'alice@example.com', 'USA'),
    ('Bob', 'bob@example.com', 'Canada'),
    ('Charlie', 'charlie@example.com', 'UK'),
    ('Diana', 'diana@example.com', 'USA');
")

# Create another sample table for orders
DBI::dbExecute(conn, "
  CREATE TABLE orders (
    order_id INTEGER PRIMARY KEY,
    customer_id INTEGER,
    product TEXT,
    amount REAL,
    order_date TEXT,
    FOREIGN KEY(customer_id) REFERENCES customers(id)
  );
")

# Insert some sample orders
DBI::dbExecute(conn, "
  INSERT INTO
    orders (customer_id, product, amount, order_date)
  VALUES
    (1, 'Widget', 19.99, '2024-01-15'),
    (1, 'Gadget', 29.99, '2024-01-17'),
    (2, 'Widget', 19.99, '2024-02-10'),
    (3, 'SuperWidget', 49.99, '2024-03-05'),
    (4, 'Gadget', 29.99, '2024-04-01'),
    (1, 'Thingamajig', 9.99, '2024-04-02');
")

# Ask LLM a question which it will answer using the SQL database:
"Who spent the most money and what products did they buy?" |>
  answer_using_sql(
    conn = conn,
    evaluate_code = TRUE,
    output_as_tool = TRUE
  ) |>
  send_prompt(oai)




#### 8 R functions as tools ####

## Custom function as tool

temperature_in_location <- function(
    location = c("Amsterdam", "Utrecht", "Enschede"),
    unit = c("Celcius", "Fahrenheit")
) {
  location <- match.arg(location)
  unit <- match.arg(unit)

  temperature_celcius <- switch(
    location,
    "Amsterdam" = 32.5,
    "Utrecht" = 19.8,
    "Enschede" = 22.7
  )

  if (unit == "Celcius") {
    return(temperature_celcius)
  } else {
    return(temperature_celcius * 9/5 + 32)
  }
}

# Generate documentation for a function
#   (based on formals, & help file if available)
docs <- tools_get_docs(temperature_in_location)
docs$description <- "Get the temperature in a location"
docs$arguments$unit$description <- "Unit in which to return the temperature"
docs$arguments$location$description <- "Location for which to return the temperature"
docs$return$description <- "The temperature in the specified location and unit"

"What is the weather in Enschede?" |>
  answer_using_tools(temperature_in_location) |>
  send_prompt(oai)


## Make LLM use a function from an R package to search Wikipedia for the answer

library(getwiki)

"What is something fun that happened in November 2024?" |>
  add_text("Summarize in one sentence.") |>
  answer_using_tools(search_wiki) |>
  send_prompt(oai)
