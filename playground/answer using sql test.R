library(DBI)
library(RSQLite)

# Create an in-memory SQLite database
conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

# Create a sample table of customers
DBI::dbExecute(
  conn,
  "
  CREATE TABLE
    customers (
      id INTEGER PRIMARY KEY,
      name TEXT,
      email TEXT,
      country TEXT
    );
  "
)

# Insert some sample customer data
DBI::dbExecute(
  conn,
  "
  INSERT INTO
    customers (name, email, country)
  VALUES
    ('Alice', 'alice@example.com', 'USA'),
    ('Bob', 'bob@example.com', 'Canada'),
    ('Charlie', 'charlie@example.com', 'UK'),
    ('Diana', 'diana@example.com', 'USA');
  "
)

# Create another sample table for orders
DBI::dbExecute(
  conn,
  "
  CREATE TABLE orders (
    order_id INTEGER PRIMARY KEY,
    customer_id INTEGER,
    product TEXT,
    amount REAL,
    order_date TEXT,
    FOREIGN KEY(customer_id) REFERENCES customers(id)
  );
  "
)

# Insert some sample orders
DBI::dbExecute(
  conn,
  "
  INSERT INTO
    orders (customer_id, product, amount, order_date)
  VALUES
    (1, 'Widget', 19.99, '2024-01-15'),
    (1, 'Gadget', 29.99, '2024-01-17'),
    (2, 'Widget', 19.99, '2024-02-10'),
    (3, 'SuperWidget', 49.99, '2024-03-05'),
    (4, 'Gadget', 29.99, '2024-04-01'),
    (1, 'Thingamajig', 9.99, '2024-04-02');
  "
)

# Now, you have a small SQLite database in memory with two tables:
# 1. customers
# 2. orders
#
# You can test SQL queries like:
# dbGetQuery(conn, "SELECT * FROM customers;")
#
# or more complex joins:
# dbGetQuery(conn, "
#  SELECT c.name, c.country, o.product, o.amount
#  FROM customers c
#  JOIN orders o ON c.id = o.customer_id
#  WHERE c.country = 'USA';
# ")

# When you are done, disconnect:
# dbDisconnect(conn)

"Do I have customers from The Netherlands?" |>
  answer_using_sql(
    conn = conn,
    evaluate_code = TRUE,
    output_as_tool = TRUE
  ) |>
  send_prompt()
