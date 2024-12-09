\dontrun{
  # Sample SQLite database to illustrate example:
  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  # Sample table of customers:
  DBI::dbExecute(conn, "
  CREATE TABLE
    customers (
      id INTEGER PRIMARY KEY,
      name TEXT,
      email TEXT,
      country TEXT
    );
  ")

  # Some sample customer data:
  DBI::dbExecute(conn, "
  INSERT INTO
    customers (name, email, country)
  VALUES
    ('Alice', 'alice@example.com', 'USA'),
    ('Bob', 'bob@example.com', 'Canada'),
    ('Charlie', 'charlie@example.com', 'UK'),
    ('Diana', 'diana@example.com', 'USA');
  ")

  # Sample table for orders:
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

  # Ask LLM a question which it will answer using the SQL database:
  "Where are my customers from?" |>
    answer_using_sql(
      conn = conn,
      evaluate_code = TRUE,
      output_as_tool = TRUE
    ) |>
    send_prompt(llm_provider_openai())
}
