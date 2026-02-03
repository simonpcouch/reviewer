#' Options used by the reviewer package
#'
#' @description
#' The reviewer package makes use of the following user-facing options:
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
#' @aliases reviewer.pending_edits
NULL
