\dontrun{
  "What would be a suitable e-mail address for cupcake company?" |>
    answer_as_regex("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$") |>
    send_prompt(llm_provider_ollama())
  # --- Sending request to LLM provider (llama3.1:8b): ---
  #   What would be a suitable e-mail address for cupcake company?
  #
  #   You must answer with a response that matches this format:
  #   ^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$.
  # --- Receiving response from LLM provider: ---
  #   A fun challenge!
  #
  #   Here are a few ideas:
  #
  #   * sweetdeals@cupcakeco.com
  #   * frostyfriends@gmail.com
  #   * sugarhighs@yumcakes.net
  #   * cupcakerush@yahoo.com
  #   * sprinkles@cupcakeparlor.org
  #
  #   But, I must say, your request is quite specific...
  #   Are you looking for an email address that matches the regex pattern exactly?
  #
  #   If so, here's a attempt:
  #
  #   `cupcakes4u@example.com`
  #
  #   This one meets the requirements: `^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$`
  #
  #   Let me know if I'm correct!
  # --- Sending request to LLM provider (llama3.1:8b): ---
  #   You must answer with a response that matches this format:
  #   ^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$.
  # --- Receiving response from LLM provider: ---
  #   sweetdeals@cupcakeco.com
  # [1] "sweetdeals@cupcakeco.com"
}
