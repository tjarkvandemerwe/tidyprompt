\dontrun{
  persona <- "Create a persona for me, please." |>
    answer_as_named_list(
      item_names = c("name", "age", "occupation"),
      item_instructions = list(
        name = "The name of the persona",
        age = "The age of the persona",
        occupation = "The occupation of the persona"
      )
    ) |> send_prompt(llm_provider_ollama())
  # --- Sending request to LLM provider (llama3.1:8b): ---
  #   Create a persona for me, please.
  #
  #   Respond with a named list like so:
  #   -- name: <<value>> (The name of the persona)
  #   -- age: <<value>> (The age of the persona)
  #   -- occupation: <<value>> (The occupation of the persona)
  #   Each name must correspond to: name, age, occupation
  # --- Receiving response from LLM provider: ---
  #   Here is your persona:
  #
  #   -- name: Astrid Welles
  #   -- age: 32
  #   -- occupation: Museum Curator
  persona$name
  # [1] "Astrid Welles"
  persona$age
  # [1] "32"
  persona$occupation
  # [1] "Museum Curator"
}
