the <- rlang::new_environment()

the$reviews <- list()
the$memory_rejected <- FALSE
the$propose_edit_count <- 0L

provider_available <- function(chat_fn) {
  tryCatch(
    {
      suppressWarnings(suppressMessages(chat_fn()))
      TRUE
    },
    error = function(e) FALSE
  )
}

prompt_provider_selection <- function() {
  providers <- list(
    list(
      name = "Anthropic (Claude Sonnet 4.5)",
      fn_name = "chat_anthropic",
      model = "claude-sonnet-4-5",
      create_client = function() {
        ellmer::chat_anthropic(model = "claude-sonnet-4-5")
      }
    ),
    list(
      name = "OpenAI (GPT 5.2)",
      fn_name = "chat_openai",
      model = "gpt-5.2",
      create_client = function() ellmer::chat_openai(model = "gpt-5.2")
    ),
    list(
      name = "Google Gemini (Gemini 3 Pro)",
      fn_name = "chat_google_gemini",
      model = "gemini-3-pro-preview",
      create_client = function() {
        ellmer::chat_google_gemini(model = "gemini-3-pro-preview")
      }
    ),
    list(
      name = "GitHub (GPT 4.1)",
      fn_name = "chat_github",
      model = "gpt-4.1",
      create_client = function() ellmer::chat_github(model = "gpt-4.1")
    )
  )

  for (i in seq_along(providers)) {
    providers[[i]]$available <- provider_available(providers[[i]]$create_client)
  }

  available_providers <- Filter(function(p) p$available, providers)

  if (length(available_providers) == 0) {
    cli::cli_abort(
      "Could not auto-discover an LLM provider, see {.help reviewer::review}.",
      call = NULL
    )
  }

  choices <- c(
    vapply(available_providers, function(p) p$name, character(1)),
    "Some other provider/model"
  )

  selection <- utils::menu(
    choices,
    title = "Which provider/model would you like to use with `review()`?"
  )

  if (selection == 0) {
    cli::cli_abort("Setup cancelled.", call = NULL)
  }

  if (selection == length(choices)) {
    cli::cli_abort(
      c(
        "Set the {.code reviewer.client} option with
         {.code options(reviewer.client = ellmer::chat_*())} or provide
         a {.arg client} argument to continue.",
        "i" = "See {.help reviewer::review} for more information."
      ),
      call = rlang::call2("reviewer::review")
    )
  }

  selected_info <- available_providers[[selection]]
  client <- selected_info$create_client()
  options(reviewer.client = client)

  prompt_persistence_selection(selected_info$fn_name, selected_info$model)

  client
}

prompt_persistence_selection <- function(fn_name, model) {
  choices <- c(
    "Just for this R session",
    "In this and future R sessions"
  )

  selection <- utils::menu(choices, title = "Store this configuration...")

  if (selection == 0 || selection == 1) {
    return(invisible(NULL))
  }

  persist_client_option(fn_name, model)
  invisible(NULL)
}

persist_client_option <- function(fn_name, model) {
  proj_rprofile <- ".Rprofile"
  use_project <- file.exists(proj_rprofile)

  rprofile_path <- if (use_project) {
    proj_rprofile
  } else {
    path.expand("~/.Rprofile")
  }

  option_line <- sprintf(
    "options(reviewer.client = ellmer::%s(model = \"%s\"))",
    fn_name,
    model
  )

  if (file.exists(rprofile_path)) {
    existing <- readLines(rprofile_path, warn = FALSE)
  } else {
    existing <- character(0)
  }

  if (any(grepl("reviewer.client", existing, fixed = TRUE))) {
    cli::cli_abort(
      "A {.code reviewer.client} option already exists in {.file {rprofile_path}}."
    )
  }

  new_content <- c(existing, "", option_line)
  writeLines(new_content, rprofile_path)

  cli::cli_alert_success(
    "Added {.code {option_line}} to {.file {rprofile_path}}."
  )
  invisible(NULL)
}

pkg_env <- function() {
  env_name <- "pkg:reviewer"

  if (env_name %in% search()) {
    return(as.environment(env_name))
  }

  env <- rlang::new_environment(parent = .GlobalEnv)
  attach(env, name = env_name, warn.conflicts = FALSE)
  env
}

set_last_review <- function(chat) {
  env <- pkg_env()
  env$.last_review <- chat
  invisible(chat)
}

get_reviewer_client <- function() {
  getOption("reviewer.client")
}

new_reviewer_chat <- function(
  client,
  system_prompt,
  call = rlang::caller_env()
) {
  client_option <- get_reviewer_client()

  if (is.null(client_option) && is.null(client)) {
    if (!interactive()) {
      cli::cli_abort(
        c(
          "!" = "Setup requires an interactive R session.",
          "i" = "Set {.code options(reviewer.client = ellmer::chat_*())} to continue.",
          "i" = "See {.help reviewer::review} for more information."
        ),
        call = NULL
      )
    }
    client_option <- prompt_provider_selection()
  }

  if (is.null(client_option)) {
    return(ellmer::chat(client, system_prompt = system_prompt, echo = "output"))
  }

  if (inherits(client_option, "Chat")) {
    client <- client_option$clone()
    client$set_system_prompt(system_prompt)
    return(client)
  }

  if (is.character(client_option) && length(client_option) == 1) {
    return(ellmer::chat(
      client_option,
      system_prompt = system_prompt,
      echo = "output"
    ))
  }

  cli::cli_abort(
    c(
      "!" = "The option {.code reviewer.client} must be an ellmer Chat object or
             a model string, not {.obj_type_friendly {client_option}}.",
      "i" = "Set e.g.
             {.code options(reviewer.client = ellmer::chat_claude(\"claude-sonnet-4-5\"))}
             or {.code options(reviewer.client = \"openai/gpt-5\")}
             in your {.file ~/.Rprofile}."
    ),
    call = call
  )
}

get_reviewer_pending_edits <- function(max_pending = NULL) {
  if (!is.null(max_pending)) {
    return(max_pending)
  }

  option_value <- getOption("reviewer.pending_edits")

  if (is.null(option_value)) {
    return(2)
  }

  if (
    is.numeric(option_value) && length(option_value) == 1 && option_value >= 1
  ) {
    return(as.integer(option_value))
  }

  cli::cli_abort(
    c(
      "!" = "The option {.code reviewer.pending_edits} must be a positive integer,
             not {.obj_type_friendly {option_value}}.",
      "i" = "Set e.g. {.code options(reviewer.pending_edits = 5)}
             in your {.file ~/.Rprofile}."
    )
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
  if (length(the$reviews) == 0) {
    return(character(0))
  }

  created_times <- sapply(the$reviews, function(r) r$created_at)
  names(the$reviews)[order(created_times)]
}

sort_reviews_by_position <- function(reviews, file_lines) {
  if (length(reviews) == 0) {
    return(reviews)
  }

  file_text <- paste(file_lines, collapse = "\n")

  positions <- vapply(
    reviews,
    function(review) {
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
    },
    numeric(1)
  )

  reviews[order(positions)]
}

get_edit_range <- function(edit_info, file_text) {
  if (isTRUE(edit_info$str_replace_mode) && !is.null(edit_info$old_str)) {
    match_pos <- regexpr(edit_info$old_str, file_text, fixed = TRUE)
    if (match_pos > 0) {
      start_char <- as.integer(match_pos)
      end_char <- start_char + nchar(edit_info$old_str) - 1L
      return(list(start = start_char, end = end_char))
    }
  } else if (!is.null(edit_info$insert_line)) {
    if (edit_info$insert_line == 0) {
      return(list(start = 0L, end = 0L))
    }
    lines <- strsplit(file_text, "\n", fixed = TRUE)[[1]]
    char_pos <- sum(nchar(lines[seq_len(edit_info$insert_line)])) +
      edit_info$insert_line
    return(list(start = char_pos, end = char_pos))
  }
  NULL
}

check_edit_conflicts <- function(new_edit_info, file_lines) {
  pending_ids <- pending_reviews()
  if (length(pending_ids) == 0) {
    return(NULL)
  }

  file_text <- paste(file_lines, collapse = "\n")
  new_range <- get_edit_range(new_edit_info, file_text)

  if (is.null(new_range)) {
    return(NULL)
  }

  for (id in pending_ids) {
    pending_edit <- the$reviews[[id]]$edit_info
    pending_range <- get_edit_range(pending_edit, file_text)

    if (is.null(pending_range)) {
      next
    }

    ranges_overlap <- new_range$start <= pending_range$end &&
      new_range$end >= pending_range$start

    if (ranges_overlap) {
      return(list(
        conflict = TRUE,
        conflicting_edit_id = id,
        conflicting_edit_intent = if (is.null(pending_edit$intent)) {
          "Edit"
        } else {
          pending_edit$intent
        }
      ))
    }
  }

  NULL
}

reset_reviews <- function() {
  the$reviews <- list()
  the$propose_edit_count <- 0L
  invisible()
}
