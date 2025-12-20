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

    # Validate shift
    if (is.null(shift)) shift <- 0

    # Get current state from session
    file_content <- session$userData$file_content
    editable_region <- session$userData$editable_region
    pending_edit <- session$userData$pending_edit
    file_path <- session$userData$file_path

    current_lines <- shiny::isolate(file_content())
    current_region <- shiny::isolate(editable_region())

    # If no new_str, this is a shift-only operation
    if (is.null(new_str)) {
      if (shift > 0) {
        # Update editable region
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

    # Validate edit mode
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

    # Validate str_replace mode
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

    # Validate insert mode
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

    # Calculate diff information for display
    diff_info <- calculate_diff_info(
      current_lines,
      old_str,
      new_str,
      insert_line,
      str_replace_mode
    )

    # Create unique request ID
    request_id <- uuid::UUIDgenerate()

    # Set up the pending edit display
    edit_info <- list(
      request_id = request_id,
      intent = `_intent` %||% "Edit",
      justification = justification %||% "",
      old_str = old_str,
      new_str = new_str,
      insert_line = insert_line,
      shift = shift,
      diff_lines = diff_info$diff_lines,
      added_lines_display = diff_info$added_lines,
      insert_after_line = diff_info$insert_after_line
    )

    pending_edit(edit_info)

    # Create promise that waits for user response
    approval_promise <- promises::promise(function(resolve, reject) {
      session$userData$edit_resolvers$set(request_id, resolve)
    })

    # Timeout after 5 minutes
    timeout_promise <- promises::promise(function(resolve, reject) {
      later::later(function() {
        resolve(list(approved = FALSE, feedback = "Timeout: No response from user."))
      }, delay = 300)
    })

    # Wait for response
    response <- coro::await(promises::promise_race(
      approval_promise,
      timeout_promise
    ))

    # Clean up
    session$userData$edit_resolvers$remove(request_id)
    pending_edit(NULL)

    # Handle response
    if (isTRUE(response$approved)) {
      # Apply the edit
      new_lines <- apply_edit_to_lines(
        current_lines,
        old_str,
        new_str,
        insert_line,
        str_replace_mode
      )

      file_content(new_lines)

      # Write to file
      writeLines(new_lines, file_path)

      # Update editable region with shift
      if (shift > 0) {
        new_start <- min(current_region$start + shift, length(new_lines))
        new_end <- min(current_region$end + shift, length(new_lines))
        editable_region(list(start = new_start, end = new_end))
      }

      return(ellmer::ContentToolResult(
        value = "Edit accepted and applied.",
        extra = list(
          display = list(
            markdown = "Edit accepted.",
            title = "Edit Applied",
            show_request = FALSE
          )
        )
      ))
    } else if (!is.null(response$feedback) && nzchar(response$feedback)) {
      return(ellmer::ContentToolResult(
        value = sprintf("User provided feedback: %s", response$feedback)
      ))
    } else {
      return(ellmer::ContentToolResult(
        value = "Edit rejected by user. Consider a different approach or move on."
      ))
    }
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
