tool_remember <- function() {
  ellmer::tool(
    remember_impl,
    name = "remember",
    description = paste(
      "Record a user preference based on their response to a proposed edit.",
      "Use this to help future agents understand what kinds of edits this user",
      "prefers or dislikes. Only call this for patterns worth remembering -",
      "not for one-off decisions or context-specific choices."
    ),
    arguments = list(
      feedback = ellmer::type_enum(
        "Whether the user accepted or rejected this type of edit.",
        values = c("accept", "reject")
      ),
      entry = ellmer::type_string(
        "Brief (<10 word), precise description of the edit type. E.g., 'Removing redundant library() calls' or 'Using seq_len() instead of 1:n'."
      )
    )
  )
}

remember_impl <- function(feedback, entry) {
  memory_path <- get_memory_path()

  if (is.null(memory_path) || !file.exists(memory_path)) {
    return(ellmer::ContentToolResult(
      value = "Memory system is not enabled for this session."
    ))
  }

  content <- readLines(memory_path, warn = FALSE)
  content_text <- paste(content, collapse = "\n")

  if (grepl(entry, content_text, fixed = TRUE)) {
    return(ellmer::ContentToolResult(
      value = "This preference is already recorded."
    ))
  }

  header <- if (feedback == "accept") {
    "### Edits approved previously"
  } else {
    "### Edits rejected previously"
  }

  header_line <- which(content == header)
  if (length(header_line) == 0) {
    content <- c(content, "", header, "", paste0("* ", entry))
  } else {
    insert_pos <- header_line + 1
    while (
      insert_pos <= length(content) && !grepl("^###", content[insert_pos])
    ) {
      insert_pos <- insert_pos + 1
    }
    content <- c(
      content[seq_len(insert_pos - 1)],
      paste0("* ", entry),
      if (insert_pos <= length(content)) content[insert_pos:length(content)]
    )
  }

  writeLines(content, memory_path)

  ellmer::ContentToolResult(
    value = sprintf("Recorded: user %ss '%s'.", feedback, entry),
    extra = list(display = list(show_request = FALSE))
  )
}

setup_memory <- function() {
  memory_option <- getOption("reviewer.memory")

  if (!is.null(memory_option)) {
    if (!is.character(memory_option) || length(memory_option) != 1) {
      cli::cli_abort(
        c(
          "!" = "The option {.code reviewer.memory} must be a file path,
                 not {.obj_type_friendly {memory_option}}.",
          "i" = "Set e.g. {.code options(reviewer.memory = \"~/.config/reviewer/memory.md\")}."
        )
      )
    }
    memory_path <- path.expand(memory_option)
    ensure_memory_file(memory_path)
    return(memory_path)
  }

  default_path <- default_memory_path()
  if (file.exists(default_path)) {
    ensure_memory_file(default_path)
    return(default_path)
  }

  if (isTRUE(the$memory_rejected)) {
    return(NULL)
  }

  if (!interactive()) {
    return(NULL)
  }

  prompt_memory_permission()
}

default_memory_path <- function(expand = TRUE) {
  path <- file.path("~", ".config", "reviewer", "memory.md")
  if (expand) path.expand(path) else path
}

prompt_memory_permission <- function() {
  default_path <- default_memory_path(expand = FALSE)

  cli::cli_inform(c(
    "i" = "{.pkg reviewer} can remember your preferences across sessions.",
    "i" = "This helps future reviews match your coding style."
  ))

  choices <- c(
    "Yes, save preferences",
    "No, don't save preferences"
  )

  selection <- utils::menu(
    choices,
    title = cli::format_inline(
      "\nAllow reviewer to write to {.path {default_path}}?"
    )
  )
  default_path <- default_memory_path()

  if (selection == 0 || selection == 2) {
    the$memory_rejected <- TRUE
    cli::cli_inform(c(
      "i" = "To enable memory later, set {.code options(reviewer.memory = \"path/to/memory.md\")}."
    ))
    return(NULL)
  }

  ensure_memory_file(default_path)
  default_path
}

ensure_memory_file <- function(path) {
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }

  if (!file.exists(path)) {
    template <- c(
      "### Edits approved previously",
      "",
      "### Edits rejected previously",
      ""
    )
    writeLines(template, path)
    return(invisible(path))
  }

  content <- readLines(path, warn = FALSE)
  content_text <- paste(content, collapse = "\n")

  headers_to_check <- c(
    "### Edits approved previously",
    "### Edits rejected previously"
  )

  for (header in headers_to_check) {
    if (!grepl(header, content_text, fixed = TRUE)) {
      content <- c(content, "", header, "")
      writeLines(content, path)
    }
  }

  invisible(path)
}

get_memory_path <- function() {
  memory_option <- getOption("reviewer.memory")
  if (!is.null(memory_option)) {
    return(path.expand(memory_option))
  }

  default_path <- default_memory_path()
  if (file.exists(default_path)) {
    return(default_path)
  }

  NULL
}

read_memory_for_prompt <- function(memory_path) {
  if (is.null(memory_path)) {
    return(NULL)
  }

  guidance <- paste0(
    "## Remembering user preferences\n\n",
    "Use the `remember()` tool to record user preferences for future sessions. ",
    "Call it when you notice a clear pattern in user behavior:\n\n",
    "* When a user consistently accepts a certain type of edit (e.g., always accepting spacing fixes)\n",
    "* When a user rejects a type of edit you've proposed (e.g., rejecting `withr::defer()` suggestions)\n\n",
    "Guidelines for using `remember()`:\n\n",
    "* Only record generalizable patterns, not one-off or context-specific decisions\n",
    "* Don't record redundant preferences - check if a similar preference already exists\n",
    "* Call `remember()` at most once every 3 `propose_edit()` calls\n",
    "* Keep entries brief and precise (under 10 words)"
  )

  if (!file.exists(memory_path)) {
    return(guidance)
  }

  content <- readLines(memory_path, warn = FALSE)
  content_text <- paste(content, collapse = "\n")
  content_text <- trimws(content_text)

  has_entries <- grepl("^\\* ", content_text, perl = TRUE) ||
    grepl("\n\\* ", content_text, perl = TRUE)

  if (!has_entries) {
    return(guidance)
  }

  preferences <- paste0(
    "## User preferences\n\n",
    "The user has interacted with similar agents previously. ",
    "Previous agents have summarized their interactions as such:\n\n",
    content_text
  )

  paste(guidance, preferences, sep = "\n\n")
}
