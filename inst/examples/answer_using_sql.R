\dontrun{
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
  "Where are my customers from?" |>
    answer_using_sql(
      conn = conn,
      evaluate_code = TRUE,
      output_as_tool = TRUE
    ) |>
    send_prompt(llm_provider_openai())
  # --- Sending request to LLM provider (gpt-4o-mini): ---
  # Where are my customers from?
  #
  # You must code in SQL to answer this prompt. You must provide all SQL code
  # between ```sql and ```.
  #
  # Never make assumptions about the possible values in the tables.
  # Instead, execute SQL queries to retrieve information you need.
  #
  # These tables are available in the database:
  #   customers, orders
  #
  # Table descriptions:
  #   - customers
  # Columns: id, name, email, country
  #
  # - orders
  # Columns: order_id, customer_id, product, amount, order_date
  #
  # Your SQL query will be executed on the database. The results will be sent back
  # to you. After seeing the results, you can either provide a final answer or try
  # another SQL query. When you provide your final answer, do not include any SQL code.
  # --- Receiving response from LLM provider: ---
  # ```sql
  # SELECT DISTINCT country FROM customers;
  # ```
  # --- Sending request to LLM provider (gpt-4o-mini): ---
  # --- SQL code: ---
  # SELECT DISTINCT country FROM customers;
  #
  # --- Query results: ---
  #   country
  # 1     USA
  # 2  Canada
  # 3      UK
  # --- Receiving response from LLM provider: ---
  # Based on the query results, your customers are from the following countries:
  # USA, Canada, and UK.
  # [1] "Based on the query results, your customers are from the following countries:
  # USA, Canada, and UK."
}
