## ----chunk_options, include = FALSE, cache=TRUE-------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, cache=TRUE--------------------------------------------------------
library(tidyprompt)

## ----llm_provider, cache=TRUE-------------------------------------------------
# Ollama running on local PC
ollama <- create_ollama_llm_provider(
  parameters = list(model = "llama3.1:8b", url = "http://localhost:11434/api/chat")
)

# OpenAI API
openai <- create_openai_llm_provider(
  parameters = list(model = "gpt-4o-mini", api_key = Sys.getenv("OPENAI_API_KEY"))
)

# Create your own LLM provider hook using create_llm_provider(); 
#   see ?create_llm_provider for more information, and take a look at
#   the source code of create_ollama_llm_provider() and create_openai_llm_provider()

