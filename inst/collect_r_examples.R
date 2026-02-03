# Script to collect R analysis files from GitHub
#
# This script searches GitHub for .R files that are NOT part of R packages
# (i.e., not in R/ directories) and saves them to inst/examples along with
# a manifest tracking source URLs and licenses.
#
# Usage: source("inst/collect_r_examples.R")
#        collect_r_examples(n = 10)

library(gh)
library(jsonlite)

#' Collect R analysis files from GitHub
#'
#' @param n Number of files to collect
#' @param output_dir Directory to save files (default: "inst/examples")
#' @param manifest_file Path to manifest file (default: "inst/examples/MANIFEST.json")
#' @param overwrite Whether to overwrite existing files (default: FALSE)
#' @param search_query Additional search query terms (default: "analysis OR script")
#'
#' @return Invisibly returns the manifest data frame
collect_r_examples <- function(
  n = 10,
  output_dir = "inst/examples",
  manifest_file = file.path(output_dir, "MANIFEST.json"),
  overwrite = FALSE,
  search_query = "analysis OR script OR data"
) {
  # Create output directory if needed

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    message("Created directory: ", output_dir)
  }

  # Load existing manifest or create new one
  manifest <- load_manifest(manifest_file)

  # Search for R files on GitHub
  message("Searching GitHub for R files...")
  candidates <- search_r_files(search_query, n_results = n * 5) # Get extra to filter

  if (length(candidates) == 0) {
    message("No candidate files found.")
    return(invisible(manifest))
  }

  message("Found ", length(candidates), " candidate files")

  # Filter out package files and already collected files
  candidates <- filter_candidates(candidates, manifest, overwrite)
  message("After filtering: ", length(candidates), " candidates remaining")

  if (length(candidates) == 0) {
    message("No new files to collect after filtering.")
    return(invisible(manifest))
  }

  # Collect files up to n
  collected <- 0
  for (candidate in candidates) {
    if (collected >= n) {
      break
    }

    result <- try(
      collect_single_file(candidate, output_dir, manifest),
      silent = TRUE
    )

    if (!inherits(result, "try-error") && !is.null(result)) {
      manifest <- result
      collected <- collected + 1
      message(sprintf("[%d/%d] Collected: %s", collected, n, candidate$name))
    }
  }

  # Save manifest
  save_manifest(manifest, manifest_file)
  message(
    "\nCollected ",
    collected,
    " files. Manifest saved to: ",
    manifest_file
  )

  invisible(manifest)
}


#' Search GitHub for R files
#'
#' @param query Search query terms
#' @param n_results Maximum number of results to return
#'
#' @return List of candidate file metadata
search_r_files <- function(query, n_results = 50) {
  # GitHub code search query
  # Exclude common package directories and test files
  search_query <- paste0(
    query,
    " extension:R",
    " NOT path:R/",
    " NOT path:tests/",
    " NOT path:test/",
    " NOT path:man/",
    " NOT path:vignettes/"
  )

  results <- list()
  page <- 1
  per_page <- min(100, n_results)

  while (length(results) < n_results && page <= 10) {
    response <- try(
      gh(
        "GET /search/code",
        q = search_query,
        per_page = per_page,
        page = page,
        .accept = "application/vnd.github.v3+json"
      ),
      silent = TRUE
    )

    if (inherits(response, "try-error")) {
      warning(
        "GitHub API error on page ",
        page,
        ": ",
        conditionMessage(attr(response, "condition"))
      )
      break
    }

    if (length(response$items) == 0) {
      break
    }

    for (item in response$items) {
      results <- c(
        results,
        list(list(
          name = item$name,
          path = item$path,
          repo_full_name = item$repository$full_name,
          repo_url = item$repository$html_url,
          file_url = item$html_url,
          raw_url = gsub(
            "github.com",
            "raw.githubusercontent.com",
            item$html_url
          ),
          sha = item$sha
        ))
      )
    }

    page <- page + 1
    Sys.sleep(1)
  }

  results[seq_len(min(length(results), n_results))]
}


#' Filter candidates to exclude package files and duplicates
#'
#' @param candidates List of candidate files
#' @param manifest Current manifest
#' @param overwrite Whether to include already collected files
#'
#' @return Filtered list of candidates
filter_candidates <- function(candidates, manifest, overwrite) {
  # Patterns that suggest the file is from a package
  pkg_patterns <- c(
    "^R/",
    "/R/",
    "^man/",
    "/man/",
    "^tests/",
    "/tests/",
    "^vignettes/",
    "/vignettes/",
    "DESCRIPTION$",
    "NAMESPACE$"
  )

  # Filenames to exclude (case-insensitive) - e.g., Shiny app files
  excluded_names <- c("ui.r", "app.r")

  filtered <- list()

  for (candidate in candidates) {
    # Skip if path matches package patterns
    is_pkg <- any(vapply(
      pkg_patterns,
      function(p) grepl(p, candidate$path),
      logical(1)
    ))
    if (is_pkg) {
      next
    }

    # Skip excluded filenames (case-insensitive)
    if (tolower(candidate$name) %in% excluded_names) {
      next
    }

    # Skip if already in manifest (unless overwrite)
    if (!overwrite && candidate$sha %in% manifest$sha) {
      next
    }
    if (!overwrite && candidate$file_url %in% manifest$file_url) {
      next
    }

    filtered <- c(filtered, list(candidate))
  }

  filtered
}


#' Collect a single file from GitHub
#'
#' @param candidate Candidate file metadata
#' @param output_dir Output directory
#' @param manifest Current manifest
#'
#' @return Updated manifest with new entry
collect_single_file <- function(candidate, output_dir, manifest) {
  # Get repository license
  license_info <- get_repo_license(candidate$repo_full_name)

  # Download file content
  content <- download_file_content(candidate)
  if (is.null(content)) {
    return(NULL)
  }

  # Generate unique filename
  safe_repo <- gsub("/", "_", candidate$repo_full_name)
  safe_name <- gsub("[^a-zA-Z0-9_.-]", "_", candidate$name)
  filename <- paste0(safe_repo, "__", safe_name)

  # Ensure .R extension
  if (!grepl("\\.R$", filename, ignore.case = TRUE)) {
    filename <- paste0(filename, ".R")
  }

  output_path <- file.path(output_dir, filename)

  # Write file content unmodified (metadata stored in manifest only)
  writeLines(content, output_path)

  # Add to manifest
  new_entry <- data.frame(
    filename = filename,
    original_name = candidate$name,
    original_path = candidate$path,
    repo_full_name = candidate$repo_full_name,
    repo_url = candidate$repo_url,
    file_url = candidate$file_url,
    sha = candidate$sha,
    license_key = license_info$license_key %||% NA_character_,
    license_name = license_info$license_name %||% NA_character_,
    license_url = license_info$license_url %||% NA_character_,
    downloaded_at = as.character(Sys.time()),
    stringsAsFactors = FALSE
  )

  rbind(manifest, new_entry)
}


#' Get repository license information
#'
#' @param repo_full_name Repository full name (owner/repo)
#'
#' @return List with license_key, license_name, license_url
get_repo_license <- function(repo_full_name) {
  response <- try(
    gh("GET /repos/{repo}", repo = repo_full_name),
    silent = TRUE
  )

  if (inherits(response, "try-error") || is.null(response$license)) {
    return(list(
      license_key = NA_character_,
      license_name = NA_character_,
      license_url = NA_character_
    ))
  }

  list(
    license_key = response$license$key %||% NA_character_,
    license_name = response$license$name %||% NA_character_,
    license_url = response$license$url %||% NA_character_
  )
}


#' Download file content from GitHub
#'
#' @param candidate Candidate file metadata
#'
#' @return File content as string, or NULL on failure
download_file_content <- function(candidate) {
  # Use the GitHub API to get file contents
  response <- try(
    gh(
      "GET /repos/{repo}/contents/{path}",
      repo = candidate$repo_full_name,
      path = candidate$path
    ),
    silent = TRUE
  )

  if (inherits(response, "try-error")) {
    warning("Failed to download: ", candidate$file_url)
    return(NULL)
  }

  # Content is base64 encoded
  if (!is.null(response$content)) {
    content <- rawToChar(base64enc::base64decode(response$content))
    return(content)
  }

  NULL
}


#' Load manifest from file
#'
#' @param manifest_file Path to manifest file
#'
#' @return Data frame with manifest entries
load_manifest <- function(manifest_file) {
  if (file.exists(manifest_file)) {
    manifest <- jsonlite::fromJSON(manifest_file)
    if (is.data.frame(manifest) && nrow(manifest) > 0) {
      return(manifest)
    }
  }

  # Return empty manifest
  data.frame(
    filename = character(),
    original_name = character(),
    original_path = character(),
    repo_full_name = character(),
    repo_url = character(),
    file_url = character(),
    sha = character(),
    license_key = character(),
    license_name = character(),
    license_url = character(),
    downloaded_at = character(),
    stringsAsFactors = FALSE
  )
}


#' Save manifest to file
#'
#' @param manifest Manifest data frame
#' @param manifest_file Path to manifest file
save_manifest <- function(manifest, manifest_file) {
  jsonlite::write_json(
    manifest,
    manifest_file,
    pretty = TRUE,
    auto_unbox = TRUE
  )
}


# Null coalescing operator
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x)) y else x
