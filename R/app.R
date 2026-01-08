#' Launch the Tidy Reviewer App
#'
#' @param file_path Path to the R file to review
#' @param model The model to use for the review (default: claude-sonnet-4-20250514)
#' @export
review <- function(
  file_path,
  model = "anthropic/claude-sonnet-4-20250514"
) {
 if (!file.exists(file_path)) {
    cli::cli_abort("File not found: {.path {file_path}}")
  }

  # Read file content
  file_lines <- readLines(file_path, warn = FALSE)

  # Read system prompt
 system_prompt <- paste(
    readLines(
      system.file("prompts/main.md", package = "reviewer"),
      warn = FALSE
    ),
    collapse = "\n"
  )


  ui <- function(req) {
    bslib::page_fillable(
      theme = bslib::bs_theme(version = 5),
      htmltools::tags$head(
      htmltools::tags$link(
        rel = "stylesheet",
        href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.0/font/bootstrap-icons.css"
      ),
      htmltools::tags$link(
        rel = "stylesheet",
        href = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/atom-one-light.min.css"
      ),
      htmltools::tags$link(rel = "stylesheet", href = "reviewer/editor.css"),
      htmltools::tags$link(rel = "stylesheet", href = "reviewer/reviewer.css"),
      htmltools::tags$script(
        src = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/highlight.min.js"
      ),
      # jQuery is included by shiny/bslib already, so scripts load after
      htmltools::tags$script(src = "reviewer/editor.js"),
      htmltools::tags$script(src = "reviewer/reviewer.js")
    ),

      bslib::layout_columns(
        col_widths = c(8, 4),
        fill = TRUE,

        # Left: Editor
        editor_mod_ui("editor"),

        # Right: Comment pane
        comment_mod_ui("comments")
      ),

      htmltools::tags$button(
        id = "close_btn",
        class = "btn-close",
        style = "position: fixed; top: 12px; right: 12px; z-index: 1000;",
        `aria-label` = "Close",
        onclick = "Shiny.setInputValue('close_btn', Math.random(), {priority: 'event'})"
      )
    )
  }

  server <- function(input, output, session) {
    shiny::addResourcePath("reviewer", system.file("www", package = "reviewer"))

    client <- ellmer::chat(model, system_prompt = system_prompt, echo = "output")
    client$register_tool(tool_propose_edit())

    reset_reviews()

    editable_region <- shiny::reactiveVal(list(start = 1, end = 15))
    file_content <- shiny::reactiveVal(file_lines)
    pending_edits <- shiny::reactiveVal(list())
    reviewing_started <- shiny::reactiveVal(FALSE)

    session$userData$editable_region <- editable_region
    session$userData$file_content <- file_content
    session$userData$pending_edits <- pending_edits
    session$userData$file_path <- file_path

    editor <- editor_mod_server(
      "editor",
      file_content = file_content,
      editable_region = editable_region,
      pending_edits = pending_edits
    )

    comments <- comment_mod_server(
      "comments",
      pending_edits = pending_edits,
      reviewing_started = reviewing_started
    )

    format_review_responses <- function() {
      unseen_ids <- unseen_resolved_reviews()
      if (length(unseen_ids) == 0) return("")

      parts <- character()
      for (id in unseen_ids) {
        review <- the$reviews[[id]]
        response <- review$response
        intent <- review$edit_info$intent

        if (isTRUE(response$approved)) {
          parts <- c(parts, sprintf("Edit '%s': Accepted and applied.", intent))
        } else if (!is.null(response$feedback) && nzchar(response$feedback)) {
          parts <- c(parts, sprintf("Edit '%s': User feedback: %s", intent, response$feedback))
        } else {
          parts <- c(parts, sprintf("Edit '%s': Rejected by user.", intent))
        }

        the$reviews[[id]]$seen_by_model <- TRUE
      }

      paste(parts, collapse = "\n")
    }

    feedback_pending <- shiny::reactiveVal(FALSE)

    start_feedback_turn <- function() {
      feedback <- format_review_responses()
      if (nzchar(feedback)) {
        current_lines <- shiny::isolate(file_content())
        current_region <- shiny::isolate(editable_region())

        file_context <- format_file_for_llm(
          current_lines,
          editable_start = current_region$start,
          editable_end = current_region$end
        )

        message <- paste0(
          "User responded to review items:\n\n",
          feedback,
          "\n\nCurrent file state:\n\n",
          file_context
        )
        stream_task$invoke(client, message)
      }
    }

    shiny::observeEvent(input$edit_response, {
      response <- input$edit_response
      request_id <- response$request_id

      if (is.null(the$reviews[[request_id]])) return()

      the$reviews[[request_id]]$status <- "resolved"
      the$reviews[[request_id]]$response <- response

      if (isTRUE(response$approved)) {
        review <- the$reviews[[request_id]]
        edit_info <- review$edit_info

        current_lines <- shiny::isolate(file_content())
        new_lines <- apply_edit_to_lines(
          current_lines,
          edit_info$old_str,
          edit_info$new_str,
          edit_info$insert_line,
          edit_info$str_replace_mode
        )
        file_content(new_lines)
        writeLines(new_lines, file_path)

        if (!is.null(edit_info$shift) && edit_info$shift > 0) {
          current_region <- shiny::isolate(editable_region())
          new_start <- min(current_region$start + edit_info$shift, length(new_lines))
          new_end <- min(current_region$end + edit_info$shift, length(new_lines))
          editable_region(list(start = new_start, end = new_end))
        }
      }

      current_lines <- shiny::isolate(file_content())
      pending_edits(sort_reviews_by_position(the$reviews[pending_reviews()], current_lines))

      if (!is.null(session$userData$throttle_resolver)) {
        session$userData$throttle_resolver(TRUE)
        session$userData$throttle_resolver <- NULL
      }

      feedback_pending(TRUE)
    })

    stream_task <- shiny::ExtendedTask$new(
      function(client, message) {
        stream <- client$stream_async(message, stream = "content")
        promises::promise_resolve(stream) |>
          promises::then(function(stream) {
            coro::async(function() {
              for (msg in stream) {
                if (promises::is.promising(msg)) {
                  msg <- coro::await(msg)
                }
                if (coro::is_exhausted(msg)) break
              }
            })()
          })
      }
    )

    shiny::observeEvent(stream_task$status(), {
      if (stream_task$status() %in% c("success", "error") && feedback_pending()) {
        feedback_pending(FALSE)
        start_feedback_turn()
      }
    })

    shiny::observeEvent(TRUE, once = TRUE, {
      reviewing_started(TRUE)

      # Format initial message with file content
      initial_message <- format_file_for_llm(
        file_lines,
        editable_start = 1,
        editable_end = 15
      )

      stream_task$invoke(client, initial_message)
    })

    # Close button handler - return the client
    shiny::observeEvent(input$close_btn, {
      shiny::stopApp(client)
    })

    # Also return client if session ends
    session$onSessionEnded(function() {
      shiny::stopApp(client)
    })
  }


  shiny::runApp(shiny::shinyApp(ui, server), launch.browser = TRUE)
}

#' Format file content for the LLM with editable region markers
#'
#' @param lines Character vector of file lines
#' @param editable_start Start line of editable region
#' @param editable_end End line of editable region
#' @param visible_lines Number of lines to show (default 50)
#' @return Formatted string for LLM consumption
format_file_for_llm <- function(
  lines,
  editable_start,
  editable_end,
  visible_lines = 50
) {
  output <- character()

  n_lines <- min(length(lines), visible_lines)

  for (i in seq_len(n_lines)) {
    if (i == editable_start) {
      output <- c(output, "{editable_region}")
    }

    line_text <- sprintf("%d: %s", i, lines[i])
    output <- c(output, line_text)

    if (i == editable_end) {
      output <- c(output, "{/editable_region}")
    }
  }

  if (visible_lines < length(lines)) {
    output <- c(output, sprintf("... (%d more lines)", length(lines) - visible_lines))
  }

  paste(output, collapse = "\n")
}
