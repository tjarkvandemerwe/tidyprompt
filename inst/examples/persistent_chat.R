# Create a persistent chat with any LLM provider
chat <- `persistent_chat-class`$new(llm_provider_ollama())

\dontrun{
  chat$chat("Hi! Tell me about Twente, in a short sentence?")
  # --- Sending request to LLM provider (llama3.1:8b): ---
  # Hi! Tell me about Twente, in a short sentence?
  # --- Receiving response from LLM provider: ---
  # Twente is a charming region in the Netherlands known for its picturesque
  # countryside and vibrant culture!

  chat$chat("How many people live there?")
  # --- Sending request to LLM provider (llama3.1:8b): ---
  # How many people live there?
  # --- Receiving response from LLM provider: ---
  # The population of Twente is approximately 650,000 inhabitants, making it one of
  # the largest regions in the Netherlands.

  # Access the chat history:
  chat$chat_history

  # Reset the chat history:
  chat$reset_chat_history()

  # Continue a chat from the result of `send_prompt()`:
  result <- "Hi there!" |>
    answer_as_integer() |>
    send_prompt(return_mode = "full")
  # --- Sending request to LLM provider (llama3.1:8b): ---
  # Hi there!
  #
  # You must answer with only an integer (use no other characters).
  # --- Receiving response from LLM provider: ---
  # 42
  chat <- `persistent_chat-class`$new(llm_provider_ollama(), result$chat_history)
  chat$chat("Why did you choose that number?")
  # --- Sending request to LLM provider (llama3.1:8b): ---
  # Why did you choose that number?
  # --- Receiving response from LLM provider: ---
  # I chose the number 42 because it's a reference to Douglas Adams' science fiction
  # series "The Hitchhiker's Guide to the Galaxy," in which a supercomputer named
  # Deep Thought is said to have calculated the "Answer to the Ultimate Question of
  # Life, the Universe, and Everything" as 42.
}

