comment_mod_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    htmltools::tags$div(
      class = "comment-pane",
      shiny::uiOutput(ns("comment_content"))
    )
  )
}

comment_mod_server <- function(
  id,
  pending_edits,
  reviewing_started
) {
  shiny::moduleServer(id, function(input, output, session) {
    output$comment_content <- shiny::renderUI({
      edits <- pending_edits()
      started <- reviewing_started()

      if (!started) {
        return(htmltools::tags$div(
          class = "reviewing-spinner",
          htmltools::tags$div(
            class = "spinner-content",
            htmltools::tags$div(class = "spinner-border spinner-border-sm text-secondary"),
            htmltools::tags$span(class = "spinner-text", "Preparing review...")
          )
        ))
      }

      if (length(edits) == 0) {
        return(htmltools::tags$div(
          class = "reviewing-spinner",
          htmltools::tags$div(
            class = "spinner-content",
            htmltools::tags$div(class = "spinner-border spinner-border-sm text-secondary"),
            htmltools::tags$span(class = "spinner-text", "Reviewing...")
          )
        ))
      }

      cards <- lapply(edits, function(review) {
        edit_info <- review$edit_info
        comment_card(
          proposal_id = edit_info$request_id,
          intent = edit_info$intent,
          justification = edit_info$justification
        )
      })

      htmltools::tagList(cards)
    })

    list()
  })
}

comment_card <- function(proposal_id, intent, justification) {
  htmltools::tags$div(
    class = "comment-card",
    `data-proposal-id` = proposal_id,

    # Header row with author and action buttons
    htmltools::tags$div(
      class = "comment-header",
      htmltools::tags$span(class = "comment-author", "Tidy Reviewer"),
      htmltools::tags$span(
        class = "comment-actions",
        htmltools::tags$button(
          class = "btn-accept",
          title = "Accept",
          htmltools::tags$i(class = "bi bi-check-lg")
        ),
        htmltools::tags$button(
          class = "btn-reject",
          title = "Reject",
          htmltools::tags$i(class = "bi bi-x-lg")
        )
      )
    ),

    # Timestamp
    htmltools::tags$div(
      class = "comment-time",
      format(Sys.time(), "%I:%M %p Today")
    ),

    # Intent as the main content (like Google Docs suggestion description)
    htmltools::tags$div(
      class = "comment-body",
      justification
    ),

    # Feedback input
    htmltools::tags$div(
      class = "comment-feedback",
      htmltools::tags$input(
        type = "text",
        class = "form-control",
        placeholder = "Reply or add feedback..."
      )
    )
  )
}
