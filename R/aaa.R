the <- rlang::new_environment()

the$reviews <- list()

get_reviewer_chat <- function() {
  getOption("reviewer.chat")
}

new_reviewer_chat <- function(model, system_prompt, call = rlang::caller_env()) {
  chat_option <- get_reviewer_chat()

  if (is.null(chat_option) && is.null(model)) {
    cli::cli_abort(
      c(
        "!" = "reviewer requires configuring an ellmer Chat with the
               {.code reviewer.chat} option or the {.arg model} argument.",
        "i" = "Set e.g.
               {.code options(reviewer.chat = ellmer::chat_claude())}
               in your {.file ~/.Rprofile} and restart R."
      ),
      call = call
    )
  }

  if (is.null(chat_option)) {
    return(ellmer::chat(model, system_prompt = system_prompt, echo = "output"))
  }

  if (inherits(chat_option, "Chat")) {
    client <- chat_option$clone()
    client$set_system_prompt(system_prompt)
    return(client)
  }

  if (is.character(chat_option) && length(chat_option) == 1) {
    return(ellmer::chat(chat_option, system_prompt = system_prompt, echo = "output"))
  }

  cli::cli_abort(
    c(
      "!" = "The option {.code reviewer.chat} must be an ellmer Chat object or
             a model string, not {.obj_type_friendly {chat_option}}.",
      "i" = "Set e.g.
             {.code options(reviewer.chat = ellmer::chat_claude())}
             or {.code options(reviewer.chat = \"openai/gpt-5\")}
             in your {.file ~/.Rprofile}."
    ),
    call = call
  )
}

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

reset_reviews <- function() {
  the$reviews <- list()
  invisible()
}
