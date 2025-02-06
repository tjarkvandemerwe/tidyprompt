msg <- "Hi, who are you?"
llm_providers <- list(
  ollama = create_ollama_llm_provider(),
  openai = create_openai_llm_provider(),
  openrouter = create_openrouter_llm_provider(),
  mistral = create_mistral_llm_provider(),
  groq = create_groq_llm_provider(),
  xai = create_xai_llm_provider(),
  gemini = create_google_gemini_llm_provider(),
  fake = create_fake_llm_provider()
)

for (i in 1:length(llm_providers)) {
  name <- names(llm_providers)[[i]]
  provider <- llm_providers[[i]]

  tryCatch(
    {
      response <- provider$complete_chat(msg)
      print(paste(name, ":", response$content))
    },
    error = function(e) {
      print(paste(name, ":", e$message))
    }
  )
}
