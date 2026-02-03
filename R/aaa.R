the <- rlang::new_environment()

the$reviews <- list()

get_reviewer_pending_edits <- function(max_pending = NULL) {
  if (!is.null(max_pending)) {
    return(max_pending)
  }

  option_value <- getOption("reviewer.pending_edits")

  if (is.null(option_value)) {
    return(3)
  }

  if (is.numeric(option_value) && length(option_value) == 1 && option_value >= 1) {
    return(as.integer(option_value))
  }

  cli::cli_abort(
    c(
      "!" = "The option {.code reviewer.pending_edits} must be a positive integer,
             not {.obj_type_friendly {option_value}}.",
      "i" = "Set e.g. {.code options(reviewer.pending_edits = 5)}
             in your {.file ~/.Rprofile}."
    )
  )
}

pending_reviews <- function() {

  names(Filter(function(r) r$status == "pending", the$reviews))
}

unseen_resolved_reviews <- function() {

  names(Filter(
    function(r) r$status == "resolved" && !r$seen_by_model,
    the$reviews
  ))
}

ordered_reviews <- function() {
  if (length(the$reviews) == 0) return(character(0))

  created_times <- sapply(the$reviews, function(r) r$created_at)
  names(the$reviews)[order(created_times)]
}

sort_reviews_by_position <- function(reviews, file_lines) {
  if (length(reviews) == 0) return(reviews)

  file_text <- paste(file_lines, collapse = "\n")

  positions <- vapply(reviews, function(review) {
    edit_info <- review$edit_info
    if (isTRUE(edit_info$str_replace_mode) && !is.null(edit_info$old_str)) {
      match_pos <- regexpr(edit_info$old_str, file_text, fixed = TRUE)
      if (match_pos > 0) {
        chars_before <- substr(file_text, 1, match_pos - 1)
        sum(charToRaw(chars_before) == charToRaw("\n")) + 1
      } else {
        Inf
      }
    } else if (!is.null(edit_info$insert_line)) {
      edit_info$insert_line
    } else {
      Inf
    }
  }, numeric(1))

  reviews[order(positions)]
}

reset_reviews <- function() {
  the$reviews <- list()
  invisible()
}
