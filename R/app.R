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


  # Create chat client with echo to see what's happening

  client <- ellmer::chat(model, system_prompt = system_prompt, echo = "output")
  client$register_tool(tool_propose_edit())

  ui <- function(req) {
    bslib::page_fillable(
      theme = bslib::bs_theme(version = 5),
      htmltools::tags$head(
        htmltools::tags$link(
          rel = "stylesheet",
          href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.0/font/bootstrap-icons.css"
        ),
        htmltools::tags$link(rel = "stylesheet", href = "reviewer/editor.css"),
        htmltools::tags$link(rel = "stylesheet", href = "reviewer/reviewer.css"),
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

      # Close button
      htmltools::tags$button(
        id = "close_btn",
        class = "btn-close",
        style = "position: fixed; top: 12px; right: 12px; z-index: 1000;",
        `aria-label` = "Close"
      )
    )
  }

  server <- function(input, output, session) {
    # Add resource path for www assets
    shiny::addResourcePath("reviewer", system.file("www", package = "reviewer"))

    # Initialize resolver storage for edit approvals
    session$userData$edit_resolvers <- fastmap::fastmap()

    # Reactive values for shared state
    editable_region <- shiny::reactiveVal(list(start = 1, end = 15))
    file_content <- shiny::reactiveVal(file_lines)
    pending_edit <- shiny::reactiveVal(NULL)
    reviewing_started <- shiny::reactiveVal(FALSE)

    # Store state in session for tool access
    session$userData$editable_region <- editable_region
    session$userData$file_content <- file_content
    session$userData$pending_edit <- pending_edit
    session$userData$file_path <- file_path

    # Initialize modules
    editor <- editor_mod_server(
      "editor",
      file_content = file_content,
      editable_region = editable_region,
      pending_edit = pending_edit
    )

    comments <- comment_mod_server(
      "comments",
      pending_edit = pending_edit,
      reviewing_started = reviewing_started
    )

    # Handle approval responses from JS
    shiny::observeEvent(input$edit_response, {
      response <- input$edit_response
      resolver <- session$userData$edit_resolvers$get(response$request_id)
      if (!is.null(resolver)) {
        resolver(response)
        session$userData$edit_resolvers$remove(response$request_id)
      }
    })

    # Create the streaming task
    stream_task <- shiny::ExtendedTask$new(
      function(client, message) {
        stream <- client$stream_async(message, stream = "content")
        promises::promise_resolve(stream) |>
          promises::then(function(stream) {
            # Consume the stream (tool calls are handled by callbacks)
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

    # Start the review on app load (once only)
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


  shiny::shinyApp(ui, server, options = list(launch.browser = TRUE))
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
