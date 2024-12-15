\dontrun{
  "What is 'Enschede'?!" |>
    answer_as_text(max_words = 50) |>
    llm_verify() |>
    send_prompt()
#   --- Sending request to LLM provider (gpt-4o-mini): ---
#   What is 'Enschede'?!
#
#   You must provide a text response. The response must be at most 50 words.
#   --- Receiving response from LLM provider: ---
#   Enschede is a city in the Netherlands, located in the eastern part near the German border.
#   It is known for its vibrant culture, history, and universities, particularly the
#   University of Twente, as well as its textiles industry and beautiful parks.
#   --- Sending request to LLM provider (gpt-4o-mini): ---
#   You are given a user's prompt.
#   To answer the user's prompt, you need to think step by step to arrive at a final answer.
#
#   ----- START OF USER'S PROMPT -----
#   >>> An assistant was asked:
#
#   What is 'Enschede'?!
#
#   >>> The assistant answered:
#
#   [1] "Enschede is a city in the Netherlands, located in the eastern part near
#   the German border. It is known for its vibrant culture, history, and universities,
#   particularly the University of Twente, as well as its textiles industry and
#   beautiful parks."
#
#   >>> Is the answer satisfactory?
#
#   You must answer with only TRUE or FALSE (use no other characters).
#   ----- END OF USER'S PROMPT -----
#
#   What are the steps you would take to answer the user's prompt?
#   Describe your thought process in the following format:
#     >> step 1: <step 1 description>
#     >> step 2: <step 2 description>
#     (etc.)
#
#   When you are done, you must type:
#     FINISH[<put here your final answer to the user's prompt>]
#
#   Make sure your final answer follows the logical conclusion of your thought process.
#   --- Receiving response from LLM provider: ---
#   >> step 1: Read the user's prompt carefully to understand what is being asked
#   regarding the assistant's answer about Enschede.
#   >> step 2: Analyze the assistant's provided answer for accuracy and completeness,
#   ensuring it conveys relevant information about Enschede.
#   >> step 3: Assess whether the answer includes key aspects, such as the city's
#   location, cultural significance, educational institutions, and any notable
#   historical references or industries tied to the city.
#   >> step 4: Determine if the assistant's answer sufficiently addresses the
#   question "What is Enschede?" and provides a clear and informative overview of the city.
#   >> step 5: Conclude whether the assistant's answer meets the standards for being
#   satisfactory in terms of detail and correctness.
#
#   FINISH[TRUE]
#   [1] "Enschede is a city in the Netherlands, located in the eastern part near the
#   German border. It is known for its vibrant culture, history, and universities,
#   particularly the University of Twente, as well as its textiles industry and beautiful parks."
}
