# new_reviewer_chat() errors when no option or model provided

    Code
      new_reviewer_chat(NULL, "system prompt")
    Condition
      Error:
      ! reviewer requires configuring an ellmer Chat with the `reviewer.chat` option or the `model` argument.
      i Set e.g. `options(reviewer.chat = ellmer::chat_claude())` in your '~/.Rprofile' and restart R.

# new_reviewer_chat() errors for invalid option type

    Code
      new_reviewer_chat(NULL, "system prompt")
    Condition
      Error:
      ! The option `reviewer.chat` must be an ellmer Chat object or a model string, not a number.
      i Set e.g. `options(reviewer.chat = ellmer::chat_claude())` or `options(reviewer.chat = "openai/gpt-5")` in your '~/.Rprofile'.

# new_reviewer_chat() errors for non-scalar string option

    Code
      new_reviewer_chat(NULL, "system prompt")
    Condition
      Error:
      ! The option `reviewer.chat` must be an ellmer Chat object or a model string, not a character vector.
      i Set e.g. `options(reviewer.chat = ellmer::chat_claude())` or `options(reviewer.chat = "openai/gpt-5")` in your '~/.Rprofile'.

