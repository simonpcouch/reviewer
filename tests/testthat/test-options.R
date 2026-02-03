test_that("get_reviewer_pending_edits() returns default when no option or argument", {
  withr::local_options(reviewer.pending_edits = NULL)
  expect_equal(get_reviewer_pending_edits(NULL), 3)
})

test_that("get_reviewer_pending_edits() uses argument when provided", {
  withr::local_options(reviewer.pending_edits = 5)
  expect_equal(get_reviewer_pending_edits(7), 7)
})

test_that("get_reviewer_pending_edits() uses option when argument is NULL", {
  withr::local_options(reviewer.pending_edits = 5)
  expect_equal(get_reviewer_pending_edits(NULL), 5)
})

test_that("get_reviewer_pending_edits() coerces numeric to integer", {
  withr::local_options(reviewer.pending_edits = 4.8)
  expect_identical(get_reviewer_pending_edits(NULL), 4L)
})

test_that("get_reviewer_pending_edits() errors for non-numeric option", {
  withr::local_options(reviewer.pending_edits = "five")
  expect_snapshot(get_reviewer_pending_edits(NULL), error = TRUE)
})

test_that("get_reviewer_pending_edits() errors for non-scalar option", {
  withr::local_options(reviewer.pending_edits = c(3, 5))
  expect_snapshot(get_reviewer_pending_edits(NULL), error = TRUE)
})

test_that("get_reviewer_pending_edits() errors for non-positive option", {
  withr::local_options(reviewer.pending_edits = 0)
  expect_snapshot(get_reviewer_pending_edits(NULL), error = TRUE)
})
