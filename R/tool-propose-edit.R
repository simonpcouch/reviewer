#' Create the propose_edit tool
#'
#' @return An ellmer tool definition
#' @noRd
tool_propose_edit <- function() {
  ellmer::tool(
    make_propose_edit_impl(),
    name = "propose_edit",
    description = paste(
      "Propose an edit to the code file being reviewed.",
      "You can either propose a text replacement/insertion, or simply shift the editable region.",
      "",
      "If you provide new_str, the edit will be shown to the user who must Accept or Reject it.",
      "If you only provide shift (without new_str), the editable region will move forward silently.",
      "",
      "The edit must be within the current editable region.",
      "Use old_str + new_str for replacements, or insert_line + new_str for insertions."
    ),
    arguments = list(
      `_intent` = ellmer::type_string(
        "A short description of the change, e.g. 'Adding spaces around equals signs'"
      ),
      justification = ellmer::type_string(
        "Your argument for why this change improves the code (2 sentences max). This is shown to the user."
      ),
      insert_line = ellmer::type_number(
        "For INSERT mode: Line number after which to insert new_str (0 = beginning of file)",
        required = FALSE
      ),
      new_str = ellmer::type_string(
        "The new text to insert or use as replacement. If omitted, only shift is applied.",
        required = FALSE
      ),
      old_str = ellmer::type_string(
        "For REPLACE mode: The exact text to find and replace. Must match exactly including whitespace.",
        required = FALSE
      ),
      shift = ellmer::type_number(
        "Number of lines to shift the editable region forward after this edit (default 0)",
        required = FALSE
      )
    )
  )
}

#' Factory to create the propose_edit implementation
#'
#' @noRd
make_propose_edit_impl <- function() {
  coro::async(function(
    `_intent` = NULL,
    justification = NULL,
    insert_line = NULL,
    new_str = NULL,
    old_str = NULL,
    shift = 0
  ) {
    session <- shiny::getDefaultReactiveDomain()

    if (is.null(session)) {
      cli::cli_abort("propose_edit requires a Shiny session")
    }

    if (is.null(shift)) shift <- 0

    file_content <- session$userData$file_content
    editable_region <- session$userData$editable_region
    pending_edits <- session$userData$pending_edits
    file_path <- session$userData$file_path

    current_lines <- shiny::isolate(file_content())
    current_region <- shiny::isolate(editable_region())

    if (is.null(new_str)) {
      if (shift > 0) {
        new_start <- min(current_region$start + shift, length(current_lines))
        new_end <- min(current_region$end + shift, length(current_lines))
        editable_region(list(start = new_start, end = new_end))

        return(ellmer::ContentToolResult(
          value = sprintf(
            "Shifted editable region by %d lines. New region: lines %d-%d.",
            shift, new_start, new_end
          ),
          extra = list(
            display = list(show_request = FALSE)
          )
        ))
      } else {
        return(ellmer::ContentToolResult(
          value = "No changes made (no new_str and shift=0)."
        ))
      }
    }

    str_replace_mode <- !is.null(old_str) && !is.null(new_str)
    insert_mode <- !is.null(insert_line) && !is.null(new_str)

    if (str_replace_mode && insert_mode) {
      return(ellmer::ContentToolResult(
        value = "Error: Cannot use both str_replace mode (old_str/new_str) and insert mode (insert_line/new_str) at the same time.",
        error = "Invalid mode combination"
      ))
    }

    if (!str_replace_mode && !insert_mode) {
      return(ellmer::ContentToolResult(
        value = "Error: Must provide either (old_str + new_str) for replacement or (insert_line + new_str) for insertion.",
        error = "Missing required arguments"
      ))
    }

    if (str_replace_mode) {
      if (old_str == new_str) {
        return(ellmer::ContentToolResult(
          value = "Error: old_str and new_str are identical. No changes to make.",
          error = "old_str and new_str are identical."
        ))
      }

      old_content_text <- paste(current_lines, collapse = "\n")
      matches <- gregexpr(old_str, old_content_text, fixed = TRUE)[[1]]

      if (length(matches) == 1 && matches[1] == -1) {
        return(ellmer::ContentToolResult(
          value = sprintf(
            "Error: No match found for old_str. Make sure the text matches exactly, including whitespace. old_str was: %s",
            substr(old_str, 1, 100)
          ),
          error = "No match found"
        ))
      }

      if (length(matches) > 1) {
        return(ellmer::ContentToolResult(
          value = sprintf(
            "Error: Found %d matches for old_str. Please provide more context to make the match unique.",
            length(matches)
          ),
          error = "Multiple matches"
        ))
      }
    }

    if (insert_mode) {
      if (insert_line < 0 || insert_line > length(current_lines)) {
        return(ellmer::ContentToolResult(
          value = sprintf(
            "Error: insert_line must be between 0 and %d.",
            length(current_lines)
          ),
          error = "Invalid insert_line"
        ))
      }
    }

    diff_info <- calculate_diff_info(
      current_lines,
      old_str,
      new_str,
      insert_line,
      str_replace_mode
    )

    request_id <- uuid::UUIDgenerate()

    edit_info <- list(
      request_id = request_id,
      intent = `_intent` %||% "Edit",
      justification = justification %||% "",
      old_str = old_str,
      new_str = new_str,
      insert_line = insert_line,
      shift = shift,
      str_replace_mode = str_replace_mode,
      diff_lines = diff_info$diff_lines,
      added_lines_display = diff_info$added_lines,
      insert_after_line = diff_info$insert_after_line
    )

    the$reviews[[request_id]] <- list(
      request_id = request_id,
      status = "pending",
      edit_info = edit_info,
      response = NULL,
      seen_by_model = FALSE,
      created_at = Sys.time()
    )

    pending_edits(sort_reviews_by_position(the$reviews[pending_reviews()], current_lines))

    n_pending <- length(pending_reviews())
    if (n_pending >= 2) {
      throttle_promise <- promises::promise(function(resolve, reject) {
        session$userData$throttle_resolver <- resolve
      })
      coro::await(throttle_promise)
    }

    return(ellmer::ContentToolResult(
      value = "Review item submitted.",
      extra = list(
        display = list(show_request = FALSE)
      )
    ))
  })
}

#' Calculate diff information for display
#'
#' @noRd
calculate_diff_info <- function(
  lines,
  old_str,
  new_str,
  insert_line,
  str_replace_mode
) {
  diff_lines <- list()
  added_lines <- NULL
  insert_after_line <- NULL

  if (str_replace_mode) {
    old_content_text <- paste(lines, collapse = "\n")
    match_pos <- regexpr(old_str, old_content_text, fixed = TRUE)

    if (match_pos > 0) {
      chars_before <- substr(old_content_text, 1, match_pos - 1)
      newlines_before <- lengths(regmatches(chars_before, gregexpr("\n", chars_before)))
      start_line <- newlines_before + 1

      old_str_trimmed <- sub("\n$", "", old_str)
      old_str_lines <- strsplit(old_str_trimmed, "\n", fixed = TRUE)[[1]]
      if (length(old_str_lines) == 0) old_str_lines <- ""
      n_old_lines <- length(old_str_lines)
      end_line <- start_line + n_old_lines - 1

      new_str_trimmed <- sub("\n$", "", new_str)
      new_str_lines <- strsplit(new_str_trimmed, "\n", fixed = TRUE)[[1]]
      if (length(new_str_lines) == 0) new_str_lines <- ""

      diff_result <- match_lines_by_similarity(
        old_str_lines,
        new_str_lines,
        start_line
      )
      diff_lines <- diff_result$diff_lines
      added_lines <- diff_result$added_lines
      if (length(added_lines) > 0) {
        insert_after_line <- end_line
      }
    }
  } else {
    new_str_lines <- strsplit(new_str, "\n", fixed = TRUE)[[1]]
    added_lines <- new_str_lines
    insert_after_line <- insert_line
  }

  list(
    diff_lines = diff_lines,
    added_lines = added_lines,
    insert_after_line = insert_after_line
  )
}

match_lines_by_similarity <- function(old_lines, new_lines, start_line) {
  diff_lines <- list()
  added_lines <- character(0)

  n_old <- length(old_lines)
  n_new <- length(new_lines)

  if (n_old == 0 && n_new == 0) {
    return(list(diff_lines = diff_lines, added_lines = added_lines))
  }

  if (n_old == 0) {
    return(list(diff_lines = diff_lines, added_lines = new_lines))
  }

  if (n_new == 0) {
    for (i in seq_len(n_old)) {
      file_line <- start_line + i - 1
      diff_lines[[as.character(file_line)]] <- list(type = "removed")
    }
    return(list(diff_lines = diff_lines, added_lines = added_lines))
  }

  sim_matrix <- matrix(0, nrow = n_old, ncol = n_new)
  for (i in seq_len(n_old)) {
    for (j in seq_len(n_new)) {
      if (old_lines[i] == new_lines[j]) {
        sim_matrix[i, j] <- 1.0
      } else {
        sim_matrix[i, j] <- line_similarity(old_lines[i], new_lines[j])
      }
    }
  }

  old_matched <- rep(FALSE, n_old)
  new_matched <- rep(FALSE, n_new)
  matches <- list()

  for (i in seq_len(n_old)) {
    if (old_lines[i] %in% new_lines[!new_matched]) {
      j <- which(!new_matched & new_lines == old_lines[i])[1]
      old_matched[i] <- TRUE
      new_matched[j] <- TRUE
      matches[[length(matches) + 1]] <- list(old_idx = i, new_idx = j, sim = 1.0)
    }
  }

  while (TRUE) {
    best_sim <- 0.4
    best_i <- NA
    best_j <- NA

    for (i in which(!old_matched)) {
      for (j in which(!new_matched)) {
        if (sim_matrix[i, j] > best_sim) {
          best_sim <- sim_matrix[i, j]
          best_i <- i
          best_j <- j
        }
      }
    }

    if (is.na(best_i)) break

    old_matched[best_i] <- TRUE
    new_matched[best_j] <- TRUE
    matches[[length(matches) + 1]] <- list(
      old_idx = best_i,
      new_idx = best_j,
      sim = best_sim
    )
  }

  for (m in matches) {
    file_line <- start_line + m$old_idx - 1
    if (m$sim < 1.0) {
      diff_lines[[as.character(file_line)]] <- list(
        type = "changed",
        old_content = old_lines[m$old_idx],
        new_content = new_lines[m$new_idx]
      )
    }
  }

  for (i in which(!old_matched)) {
    file_line <- start_line + i - 1
    diff_lines[[as.character(file_line)]] <- list(type = "removed")
  }

  added_lines <- new_lines[!new_matched]

  list(diff_lines = diff_lines, added_lines = added_lines)
}

line_similarity <- function(a, b) {
  if (nchar(a) == 0 && nchar(b) == 0) return(1)
  if (nchar(a) == 0 || nchar(b) == 0) return(0)

  a_chars <- strsplit(a, "")[[1]]
  b_chars <- strsplit(b, "")[[1]]

  prefix_len <- 0
  min_len <- min(length(a_chars), length(b_chars))
  while (prefix_len < min_len && a_chars[prefix_len + 1] == b_chars[prefix_len + 1]) {
    prefix_len <- prefix_len + 1
  }

  suffix_len <- 0
  while (suffix_len < (min_len - prefix_len) &&
         a_chars[length(a_chars) - suffix_len] == b_chars[length(b_chars) - suffix_len]) {
    suffix_len <- suffix_len + 1
  }

  common_len <- prefix_len + suffix_len
  common_len / min(length(a_chars), length(b_chars))
}

#' Apply edit to file lines
#'
#' @noRd
apply_edit_to_lines <- function(
  lines,
  old_str,
  new_str,
  insert_line,
  str_replace_mode
) {
  if (str_replace_mode) {
    old_content_text <- paste(lines, collapse = "\n")
    new_content_text <- sub(old_str, new_str, old_content_text, fixed = TRUE)
    strsplit(new_content_text, "\n", fixed = TRUE)[[1]]
  } else {
    # Insert mode
    new_str_lines <- strsplit(new_str, "\n", fixed = TRUE)[[1]]
    c(
      if (insert_line > 0) lines[1:insert_line] else character(0),
      new_str_lines,
      if (insert_line < length(lines)) lines[(insert_line + 1):length(lines)] else character(0)
    )
  }
}

# Use rlang's null-coalescing operator
`%||%` <- rlang::`%||%`
