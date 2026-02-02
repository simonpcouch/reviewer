#' Editor Module UI
#'
#' @param id Module ID
#' @return Shiny UI element
#' @noRd
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

render_inline_diff <- function(old_line, new_line) {
  old_chars <- strsplit(old_line, "")[[1]]
  new_chars <- strsplit(new_line, "")[[1]]

  if (length(old_chars) == 0) old_chars <- character(0)
  if (length(new_chars) == 0) new_chars <- character(0)

  prefix_len <- 0

  min_len <- min(length(old_chars), length(new_chars))
  while (prefix_len < min_len && old_chars[prefix_len + 1] == new_chars[prefix_len + 1]) {
    prefix_len <- prefix_len + 1
  }

  suffix_len <- 0
  while (suffix_len < (min_len - prefix_len) &&
         old_chars[length(old_chars) - suffix_len] == new_chars[length(new_chars) - suffix_len]) {
    suffix_len <- suffix_len + 1
  }

  prefix <- if (prefix_len > 0) paste(old_chars[1:prefix_len], collapse = "") else ""
  suffix <- if (suffix_len > 0) paste(old_chars[(length(old_chars) - suffix_len + 1):length(old_chars)], collapse = "") else ""

  old_middle_end <- length(old_chars) - suffix_len
  old_middle <- if (old_middle_end >= prefix_len + 1) {
    paste(old_chars[(prefix_len + 1):old_middle_end], collapse = "")
  } else ""

  new_middle_end <- length(new_chars) - suffix_len
  new_middle <- if (new_middle_end >= prefix_len + 1) {
    paste(new_chars[(prefix_len + 1):new_middle_end], collapse = "")
  } else ""

  html_parts <- htmltools::htmlEscape(prefix)

  if (nchar(old_middle) > 0) {
    html_parts <- paste0(
      html_parts,
      '<span class="diff-inline-removed">',
      htmltools::htmlEscape(old_middle),
      '</span>'
    )
  }
  if (nchar(new_middle) > 0) {
    html_parts <- paste0(
      html_parts,
      '<span class="diff-inline-added">',
      htmltools::htmlEscape(new_middle),
      '</span>'
    )
  }

  html_parts <- paste0(html_parts, htmltools::htmlEscape(suffix))

  html_parts
}

#' Editor Module Server
#'
#' @param id Module ID
#' @param file_content Reactive value containing file lines
#' @param editable_region Reactive value containing list(start, end)
#' @param pending_edits Reactive value containing list of pending edits
#' @return List of module functions
#' @noRd
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

        if (!is.null(edit_info$diff_lines)) {
          for (line_num in names(edit_info$diff_lines)) {
            all_diff_lines[[line_num]] <- edit_info$diff_lines[[line_num]]
          }
        }
        if (!is.null(edit_info$added_lines_display) && !is.null(edit_info$insert_after_line)) {
          all_additions[[length(all_additions) + 1]] <- list(
            insert_after = edit_info$insert_after_line,
            lines = edit_info$added_lines_display,
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

        diff_info <- all_diff_lines[[as.character(i)]]

        if (!is.null(diff_info) && diff_info$type == "changed") {
          line_content <- render_inline_diff(
            diff_info$old_content,
            diff_info$new_content
          )
          htmltools::tags$div(
            class = paste(c(classes, "diff-changed"), collapse = " "),
            `data-line` = i,
            htmltools::tags$span(class = "line-number", i),
            htmltools::tags$span(class = "line-content", htmltools::HTML(line_content))
          )
        } else {
          line_classes <- classes
          if (!is.null(diff_info)) {
            line_classes <- c(line_classes, paste0("diff-", diff_info$type))
          }
          htmltools::tags$div(
            class = paste(line_classes, collapse = " "),
            `data-line` = i,
            htmltools::tags$span(class = "line-number", i),
            htmltools::tags$span(class = "line-content", lines[i])
          )
        }
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
