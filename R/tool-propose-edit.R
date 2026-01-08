#' Create the propose_edit tool
#'
#' @return An ellmer tool definition
#' @export
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
      path = ellmer::type_string(
        "Path to the file being edited (should match the file being reviewed)"
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
      `_intent` = ellmer::type_string(
        "A short description of the change, e.g. 'Adding spaces around equals signs'"
      ),
      justification = ellmer::type_string(
        "Your argument for why this change improves the code (2 sentences max). This is shown to the user."
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
#' @keywords internal
make_propose_edit_impl <- function() {
  coro::async(function(
    path,
    insert_line = NULL,
    new_str = NULL,
    old_str = NULL,
    `_intent` = NULL,
    justification = NULL,
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
    if (n_pending > 3) {
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
#' @keywords internal
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
    # Find where the old_str starts
    old_content_text <- paste(lines, collapse = "\n")
    match_pos <- regexpr(old_str, old_content_text, fixed = TRUE)

    if (match_pos > 0) {
      chars_before <- substr(old_content_text, 1, match_pos - 1)
      # Count newlines before match to determine starting line
      newlines_before <- lengths(regmatches(chars_before, gregexpr("\n", chars_before)))
      start_line <- newlines_before + 1

      # Count actual content lines in old_str (ignore trailing newline)
      old_str_trimmed <- sub("\n$", "", old_str)
      old_str_lines <- strsplit(old_str_trimmed, "\n", fixed = TRUE)[[1]]
      n_old_lines <- max(1, length(old_str_lines))
      end_line <- start_line + n_old_lines - 1

      # Mark removed lines
      for (i in start_line:end_line) {
        diff_lines[[as.character(i)]] <- list(type = "removed")
      }

      # Added lines to show - insert AFTER the removed lines (standard diff format)
      new_str_trimmed <- sub("\n$", "", new_str)
      new_str_lines <- strsplit(new_str_trimmed, "\n", fixed = TRUE)[[1]]
      added_lines <- new_str_lines
      insert_after_line <- end_line
    }
  } else {
    # Insert mode
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

#' Apply edit to file lines
#'
#' @keywords internal
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
