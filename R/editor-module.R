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
#' @param pending_edits Reactive value containing list of pending edits
#' @return List of module functions
editor_mod_server <- function(
  id,
  file_content,
  editable_region,
  pending_edits
) {
  shiny::moduleServer(id, function(input, output, session) {

    output$file_editor <- shiny::renderUI({
      lines <- file_content()
      region <- editable_region()
      edits <- pending_edits()

      if (length(lines) == 0) {
        return(htmltools::tags$div(
          class = "editor-empty",
          "No file loaded"
        ))
      }

      all_diff_lines <- list()
      all_additions <- list()

      for (review in edits) {
        edit_info <- review$edit_info

        diff_info <- calculate_diff_info(
          lines,
          edit_info$old_str,
          edit_info$new_str,
          edit_info$insert_line,
          edit_info$str_replace_mode
        )

        if (!is.null(diff_info$diff_lines)) {
          for (line_num in names(diff_info$diff_lines)) {
            all_diff_lines[[line_num]] <- diff_info$diff_lines[[line_num]]
          }
        }
        if (!is.null(diff_info$added_lines) && !is.null(diff_info$insert_after_line)) {
          all_additions[[length(all_additions) + 1]] <- list(
            insert_after = diff_info$insert_after_line,
            lines = diff_info$added_lines,
            request_id = edit_info$request_id
          )
        }
      }

      line_tags <- lapply(seq_along(lines), function(i) {
        in_region <- i >= region$start && i <= region$end
        classes <- c(
          "editor-line",
          if (in_region) "in-region" else "dimmed"
        )

        line_classes <- classes
        diff_info <- all_diff_lines[[as.character(i)]]
        if (!is.null(diff_info)) {
          line_classes <- c(line_classes, paste0("diff-", diff_info$type))
        }

        htmltools::tags$div(
          class = paste(line_classes, collapse = " "),
          `data-line` = i,
          htmltools::tags$span(class = "line-number", i),
          htmltools::tags$span(class = "line-content", lines[i])
        )
      })

      if (length(all_additions) > 0) {
        all_additions <- all_additions[order(sapply(all_additions, `[[`, "insert_after"), decreasing = TRUE)]

        for (addition in all_additions) {
          insert_pos <- addition$insert_after
          if (!is.null(insert_pos) && insert_pos <= length(lines)) {
            added_tags <- lapply(seq_along(addition$lines), function(j) {
              htmltools::tags$div(
                class = "editor-line diff-added",
                `data-line` = paste0(insert_pos, "+", j),
                `data-request-id` = addition$request_id,
                htmltools::tags$span(class = "line-number diff-marker", "+"),
                htmltools::tags$span(class = "line-content", addition$lines[j])
              )
            })

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
      }

      htmltools::tagList(line_tags)
    })

    list(
      scroll_to_line = function(line_num) {
        session$sendCustomMessage(
          "scroll-to-line",
          list(line = line_num)
        )
      }
    )
  })
}
