\dontrun{
  "What would be a suitable e-mail address for cupcake company?" |>
    answer_as_regex("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$") |>
    send_prompt(llm_provider_ollama())
  # --- Sending request to LLM provider (llama3.1:8b): ---
  #   What would be a suitable e-mail address for cupcake company?
  #
  #   You must answer with a response that matches this regex format:
  #     ^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$
  #     (use no other characters)
  # --- Receiving response from LLM provider: ---
  #   sweet.treats.cupcakes@gmail.com
  # [1] "sweet.treats.cupcakes@gmail.com"

  "What would be a suitable e-mail address for cupcake company?" |>
    add_text("Give three ideas.") |>
    answer_as_regex(
      "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}",
      mode = "extract_matches"
    ) |>
    send_prompt(llm_provider_ollama())
  # --- Sending request to LLM provider (llama3.1:8b): ---
  #   What would be a suitable e-mail address for cupcake company?
  #
  #   Give three ideas.
  #
  #   You must answer with a response that matches this regex format:
  #     [a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}
  # --- Receiving response from LLM provider: ---
  #   Here are three potential email addresses for a cupcake company:
  #
  #   1. sweettreats.cupcakes@yummail.com
  #   2. cupcakes.and.love@flourpower.net
  #   3. thecupcakery@gmail.com
  # [1] "sweettreats.cupcakes@yummail.com" "cupcakes.and.love@flourpower.net"
  # "thecupcakery@gmail.com"
}
