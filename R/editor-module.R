#' Editor Module UI
#'
#' @param id Module ID
#' @return Shiny UI element
editor_mod_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    htmltools::tags$div(
      class = "editor-wrapper",
      htmltools::tags$div(
        class = "editor-container",
        shiny::uiOutput(ns("file_editor"))
      )
    )
  )
}

#' Editor Module Server
#'
#' @param id Module ID
#' @param file_content Reactive value containing file lines
#' @param editable_region Reactive value containing list(start, end)
#' @param pending_edit Reactive value containing pending edit info or NULL
#' @return List of module functions
editor_mod_server <- function(
  id,
  file_content,
  editable_region,
  pending_edit
) {
  shiny::moduleServer(id, function(input, output, session) {

    output$file_editor <- shiny::renderUI({
      lines <- file_content()
      region <- editable_region()
      edit <- pending_edit()

      if (length(lines) == 0) {
        return(htmltools::tags$div(
          class = "editor-empty",
          "No file loaded"
        ))
      }

      # Build line elements
      line_tags <- lapply(seq_along(lines), function(i) {
        in_region <- i >= region$start && i <= region$end
        classes <- c(
          "editor-line",
          if (in_region) "in-region" else "dimmed"
        )

        # Check if this line is part of a pending edit
        line_classes <- classes
        if (!is.null(edit) && !is.null(edit$diff_lines)) {
          diff_info <- edit$diff_lines[[as.character(i)]]
          if (!is.null(diff_info)) {
            line_classes <- c(line_classes, paste0("diff-", diff_info$type))
          }
        }

        htmltools::tags$div(
          class = paste(line_classes, collapse = " "),
          `data-line` = i,
          htmltools::tags$span(class = "line-number", i),
          htmltools::tags$span(class = "line-content", lines[i])
        )
      })

      # If there's a pending edit with added lines, insert them
      if (!is.null(edit) && !is.null(edit$added_lines_display)) {
        # Find position to insert added lines
        insert_pos <- edit$insert_after_line
        if (!is.null(insert_pos) && insert_pos <= length(line_tags)) {
          # Create added line elements
          added_tags <- lapply(seq_along(edit$added_lines_display), function(j) {
            htmltools::tags$div(
              class = "editor-line diff-added",
              `data-line` = paste0(insert_pos, "+", j),
              htmltools::tags$span(class = "line-number diff-marker", "+"),
              htmltools::tags$span(class = "line-content", edit$added_lines_display[j])
            )
          })

          # Insert after the specified position
          if (insert_pos == 0) {
            line_tags <- c(added_tags, line_tags)
          } else {
            line_tags <- c(
              line_tags[1:insert_pos],
              added_tags,
              if (insert_pos < length(line_tags)) line_tags[(insert_pos + 1):length(line_tags)] else list()
            )
          }
        }
      }

      htmltools::tagList(line_tags)
    })

    # Return module interface
    list(
      # Function to scroll to a specific line
      scroll_to_line = function(line_num) {
        session$sendCustomMessage(
          "scroll-to-line",
          list(line = line_num)
        )
      }
    )
  })
}
