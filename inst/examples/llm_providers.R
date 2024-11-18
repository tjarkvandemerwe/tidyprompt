# Various providers:
ollama <- llm_provider_ollama()
openai <- llm_provider_openai()
openrouter <- llm_provider_openrouter()
mistral <- llm_provider_mistral()
groq <- llm_provider_groq()
xai <- llm_provider_xai()
gemini <- llm_provider_google_gemini()

# Initialize with settings:
ollama <- llm_provider_ollama(
  parameters = list(
    model = "llama3.2:3b",
    stream = TRUE
  ),
  verbose = TRUE,
  url = "http://localhost:11434/api/chat"
)

# Change settings:
ollama$verbose <- FALSE
ollama$parameters$stream <- FALSE
ollama$parameters$model <- "llama3.1:8b"

\dontrun{
  # Try a simple chat message with '$complete_chat()':
  response <- ollama$complete_chat("Hi!")
  response
  # $role
  # [1] "assistant"
  #
  # $content
  # [1] "How's it going? Is there something I can help you with or would you like
  # to chat?"
  #
  # $http
  # Response [http://localhost:11434/api/chat]
  # Date: 2024-11-18 14:21
  # Status: 200
  # Content-Type: application/json; charset=utf-8
  # Size: 375 B

  # Use with send_prompt():
  "Hi" |>
    send_prompt(ollama)
  # [1] "How's your day going so far? Is there something I can help you with or
  # would you like to chat?"
}
