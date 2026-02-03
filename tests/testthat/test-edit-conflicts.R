test_that("get_edit_range returns correct range for str_replace_mode", {
  file_text <- "line one\nline two\nline three"

  edit_info <- list(
    old_str = "line two",
    str_replace_mode = TRUE
  )
  result <- get_edit_range(edit_info, file_text)

  expect_equal(result$start, 10L)
  expect_equal(result$end, 17L)
})

test_that("get_edit_range returns correct range for multi-line replacement", {
  file_text <- "line one\nline two\nline three"

  edit_info <- list(
    old_str = "line two\nline three",
    str_replace_mode = TRUE
  )
  result <- get_edit_range(edit_info, file_text)

  expect_equal(result$start, 10L)
  expect_equal(result$end, 28L)
})

test_that("get_edit_range returns NULL when old_str not found", {
  file_text <- "line one\nline two\nline three"

  edit_info <- list(
    old_str = "not found",
    str_replace_mode = TRUE
  )
  result <- get_edit_range(edit_info, file_text)

  expect_null(result)
})

test_that("get_edit_range returns correct position for insert_line", {
  file_text <- "line one\nline two\nline three"

  edit_info <- list(
    insert_line = 1,
    str_replace_mode = FALSE
  )
  result <- get_edit_range(edit_info, file_text)

  expect_equal(result$start, 9L)
  expect_equal(result$end, 9L)
})
test_that("get_edit_range returns 0 for insert at beginning", {
  file_text <- "line one\nline two"

  edit_info <- list(
    insert_line = 0,
    str_replace_mode = FALSE
  )

  result <- get_edit_range(edit_info, file_text)

  expect_equal(result$start, 0L)
  expect_equal(result$end, 0L)
})

test_that("get_edit_range returns NULL for missing edit info", {
  file_text <- "line one\nline two"

  edit_info <- list(str_replace_mode = FALSE)
  result <- get_edit_range(edit_info, file_text)

  expect_null(result)
})

test_that("check_edit_conflicts returns NULL when no pending reviews", {
  reset_reviews()

  file_lines <- c("line one", "line two", "line three")
  new_edit <- list(
    old_str = "line two",
    str_replace_mode = TRUE
  )

  result <- check_edit_conflicts(new_edit, file_lines)
  expect_null(result)
})

test_that("check_edit_conflicts detects overlapping replacements", {
  reset_reviews()

  the$reviews[["existing-id"]] <- list(
    request_id = "existing-id",
    status = "pending",
    edit_info = list(
      old_str = "line two",
      str_replace_mode = TRUE,
      intent = "Existing edit"
    )
  )

  file_lines <- c("line one", "line two", "line three")
  new_edit <- list(
    old_str = "two",
    str_replace_mode = TRUE
  )

  result <- check_edit_conflicts(new_edit, file_lines)

  expect_true(result$conflict)
  expect_equal(result$conflicting_edit_id, "existing-id")
  expect_equal(result$conflicting_edit_intent, "Existing edit")
})

test_that("check_edit_conflicts allows non-overlapping edits", {
  reset_reviews()

  the$reviews[["existing-id"]] <- list(
    request_id = "existing-id",
    status = "pending",
    edit_info = list(
      old_str = "line one",
      str_replace_mode = TRUE,
      intent = "Existing edit"
    )
  )

  file_lines <- c("line one", "line two", "line three")
  new_edit <- list(
    old_str = "line three",
    str_replace_mode = TRUE
  )


  result <- check_edit_conflicts(new_edit, file_lines)
  expect_null(result)
})

test_that("check_edit_conflicts ignores resolved reviews", {
  reset_reviews()

  the$reviews[["resolved-id"]] <- list(
    request_id = "resolved-id",
    status = "resolved",
    edit_info = list(
      old_str = "line two",
      str_replace_mode = TRUE,
      intent = "Resolved edit"
    )
  )

  file_lines <- c("line one", "line two", "line three")
  new_edit <- list(
    old_str = "line two",
    str_replace_mode = TRUE
  )

  result <- check_edit_conflicts(new_edit, file_lines)
  expect_null(result)
})

test_that("check_edit_conflicts uses default intent when missing", {
  reset_reviews()

  the$reviews[["existing-id"]] <- list(
    request_id = "existing-id",
    status = "pending",
    edit_info = list(
      old_str = "line two",
      str_replace_mode = TRUE
    )
  )

  file_lines <- c("line one", "line two", "line three")
  new_edit <- list(
    old_str = "line two",
    str_replace_mode = TRUE
  )

  result <- check_edit_conflicts(new_edit, file_lines)

  expect_equal(result$conflicting_edit_intent, "Edit")
})

test_that("check_edit_conflicts detects insert conflicting with replacement", {
  reset_reviews()

  the$reviews[["existing-id"]] <- list(
    request_id = "existing-id",
    status = "pending",
    edit_info = list(
      old_str = "line one\nline two",
      str_replace_mode = TRUE,
      intent = "Multi-line edit"
    )
  )

  file_lines <- c("line one", "line two", "line three")
  new_edit <- list(
    insert_line = 1,
    str_replace_mode = FALSE
  )

  result <- check_edit_conflicts(new_edit, file_lines)

  expect_true(result$conflict)
})
