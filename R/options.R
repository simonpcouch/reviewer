#' Options used by the reviewer package
#'
#' @description
#' The reviewer package makes use of the following user-facing options:
#'
#' * `reviewer.chat` supplies the underlying LLM powering the reviewer.
#'   When set, it takes precedence over the `model` argument to [review()].
#'   The option can be either:
#'   - An [ellmer::Chat] object (e.g., `ellmer::chat_claude()`)
#'   - A model string in `"provider/model"` format as described in [ellmer::chat()]
#'     (e.g., `"anthropic/claude-sonnet-4-5-20250929"`)
#'
#'   Typically set in your `.Rprofile`:
#'   ```
#'   options(reviewer.chat = ellmer::chat_claude("claude-sonnet-4-5-20250514"))
#'   # or
#'   options(reviewer.chat = "openai/gpt-5")
#'   ```
#'
#' * `reviewer.pending_edits` controls the maximum number of pending edits
#'   allowed at once before the model waits for user responses. When set, it
#'   takes precedence over the `max_pending` argument to [review()]. The option
#'   must be a positive integer. Higher values reduce wait time but may feel
#'   more overwhelming and risk edit conflicts. Defaults to 3.
#'
#'   Typically set in your `.Rprofile`:
#'   ```
#'   options(reviewer.pending_edits = 5)
#'   ```
#'
#' @name reviewer_options
#' @aliases reviewer.chat reviewer.pending_edits
NULL
