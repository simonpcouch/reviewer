the <- rlang::new_environment()

the$reviews <- list()

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

get_edit_range <- function(edit_info, file_text) {
  if (isTRUE(edit_info$str_replace_mode) && !is.null(edit_info$old_str)) {
    match_pos <- regexpr(edit_info$old_str, file_text, fixed = TRUE)
    if (match_pos > 0) {
      start_char <- as.integer(match_pos)
      end_char <- start_char + nchar(edit_info$old_str) - 1L
      return(list(start = start_char, end = end_char))
    }
  } else if (!is.null(edit_info$insert_line)) {
    if (edit_info$insert_line == 0) {
      return(list(start = 0L, end = 0L))
    }
    lines <- strsplit(file_text, "\n", fixed = TRUE)[[1]]
    char_pos <- sum(nchar(lines[seq_len(edit_info$insert_line)])) +
      edit_info$insert_line
    return(list(start = char_pos, end = char_pos))
  }
  NULL
}

check_edit_conflicts <- function(new_edit_info, file_lines) {
  pending_ids <- pending_reviews()
  if (length(pending_ids) == 0) {
    return(NULL)

  }

  file_text <- paste(file_lines, collapse = "\n")
  new_range <- get_edit_range(new_edit_info, file_text)

  if (is.null(new_range)) {
    return(NULL)
  }

  for (id in pending_ids) {
    pending_edit <- the$reviews[[id]]$edit_info
    pending_range <- get_edit_range(pending_edit, file_text)

    if (is.null(pending_range)) next

    ranges_overlap <- new_range$start <= pending_range$end &&
      new_range$end >= pending_range$start

    if (ranges_overlap) {
      return(list(
        conflict = TRUE,
        conflicting_edit_id = id,
        conflicting_edit_intent = if (is.null(pending_edit$intent)) "Edit" else pending_edit$intent
      ))
    }
  }

  NULL
}

reset_reviews <- function() {
  the$reviews <- list()
  invisible()
}
