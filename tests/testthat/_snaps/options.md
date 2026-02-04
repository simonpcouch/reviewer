# new_reviewer_chat() errors when no option or model provided

    Code
      new_reviewer_chat(NULL, "system prompt")
    Condition
      Error:
      ! reviewer requires configuring an ellmer Chat with the `reviewer.client` option or the `model` argument.
      i Set e.g. `options(reviewer.client = ellmer::chat_claude("claude-sonnet-4-5"))` in your '~/.Rprofile' and restart R.

# new_reviewer_chat() errors for invalid option type

    Code
      new_reviewer_chat(NULL, "system prompt")
    Condition
      Error:
      ! The option `reviewer.client` must be an ellmer Chat object or a model string, not a number.
      i Set e.g. `options(reviewer.client = ellmer::chat_claude("claude-sonnet-4-5"))` or `options(reviewer.client = "openai/gpt-5")` in your '~/.Rprofile'.

# new_reviewer_chat() errors for non-scalar string option

    Code
      new_reviewer_chat(NULL, "system prompt")
    Condition
      Error:
      ! The option `reviewer.client` must be an ellmer Chat object or a model string, not a character vector.
      i Set e.g. `options(reviewer.client = ellmer::chat_claude("claude-sonnet-4-5"))` or `options(reviewer.client = "openai/gpt-5")` in your '~/.Rprofile'.

# get_reviewer_pending_edits() errors for non-numeric option

    Code
      get_reviewer_pending_edits(NULL)
    Condition
      Error in `get_reviewer_pending_edits()`:
      ! The option `reviewer.pending_edits` must be a positive integer, not a string.
      i Set e.g. `options(reviewer.pending_edits = 5)` in your '~/.Rprofile'.

# get_reviewer_pending_edits() errors for non-scalar option

    Code
      get_reviewer_pending_edits(NULL)
    Condition
      Error in `get_reviewer_pending_edits()`:
      ! The option `reviewer.pending_edits` must be a positive integer, not a double vector.
      i Set e.g. `options(reviewer.pending_edits = 5)` in your '~/.Rprofile'.

# get_reviewer_pending_edits() errors for non-positive option

    Code
      get_reviewer_pending_edits(NULL)
    Condition
      Error in `get_reviewer_pending_edits()`:
      ! The option `reviewer.pending_edits` must be a positive integer, not a number.
      i Set e.g. `options(reviewer.pending_edits = 5)` in your '~/.Rprofile'.

