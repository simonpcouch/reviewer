#' Options used by the reviewer package
#'
#' @description
#' The reviewer package makes use of the following user-facing options:
#'
#' * `reviewer.chat` supplies the underlying LLM powering the reviewer.
#'   When set, it takes precedence over the `client` argument to [review()].
#'   The option can be either:
#'   - An [ellmer::Chat] object (e.g., `ellmer::chat_claude()`)
#'   - A model string in `"provider/model"` format as described in [ellmer::chat()]
#'     (e.g., `"anthropic/claude-sonnet-4-5"`)
#'
#'   Typically set in your `.Rprofile`:
#'   ```
#'   options(reviewer.chat = ellmer::chat_claude(model = "claude-sonnet-4-5"))
#'   # or
#'   options(reviewer.chat = "openai/gpt-5")
#'   ```
#'
#' * `reviewer.pending_edits` controls the maximum number of pending edits
#'   allowed at once before the model waits for user responses. The
#'   `max_pending` argument to [review()] takes precedence over this option
#'   when provided. The option must be a positive integer. Higher values reduce
#'   wait time but may feel more overwhelming and risk edit conflicts.
#'   Defaults to 2.
#'
#'   Typically set in your `.Rprofile`:
#'   ```
#'   options(reviewer.pending_edits = 5)
#'   ```
#'
#' * `reviewer.memory` specifies the path to a file where reviewer stores
#'   user preferences learned from accepted/rejected edits. When set, reviewer
#'   uses this file without prompting. When not set, reviewer will ask for
#'   permission to write to `~/.config/reviewer/memory.md` on first launch.
#'
#'   Typically set in your `.Rprofile`:
#'   ```
#'   options(reviewer.memory = "~/.config/reviewer/memory.md")
#'   ```
#'
#' @name reviewer_options
#' @aliases reviewer.chat reviewer.pending_edits reviewer.memory
NULL
