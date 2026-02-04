# Tests for reviewer.client option

test_that("get_reviewer_client() returns NULL when option not set", {
  withr::local_options(reviewer.client = NULL)
  expect_null(get_reviewer_client())
})

test_that("get_reviewer_client() returns the option value", {
  withr::local_options(reviewer.client = "test-model")
  expect_equal(get_reviewer_client(), "test-model")
})

test_that("new_reviewer_chat() errors when no option or model provided", {
  withr::local_options(reviewer.client = NULL)
  expect_snapshot(new_reviewer_chat(NULL, "system prompt"), error = TRUE)
})

test_that("new_reviewer_chat() uses model argument when option not set", {
  withr::local_options(reviewer.client = NULL)
  testthat::local_mocked_bindings(
    chat = function(model, ...) {
      list(model = model, args = list(...))
    },
    .package = "ellmer"
  )

  result <- new_reviewer_chat("anthropic/claude-sonnet", "test prompt")
  expect_equal(result$model, "anthropic/claude-sonnet")
  expect_equal(result$args$system_prompt, "test prompt")
})

test_that("new_reviewer_chat() uses string option over model argument", {
  withr::local_options(reviewer.client = "openai/gpt-5")
  testthat::local_mocked_bindings(
    chat = function(model, ...) {
      list(model = model, args = list(...))
    },
    .package = "ellmer"
  )

  result <- new_reviewer_chat("anthropic/claude-sonnet", "test prompt")
  expect_equal(result$model, "openai/gpt-5")
})

test_that("new_reviewer_chat() clones Chat object and sets system prompt", {
  cloned_chat <- new.env()
  cloned_chat$system_prompt <- NULL
  cloned_chat$set_system_prompt <- function(prompt) {
    cloned_chat$system_prompt <<- prompt
  }

  mock_chat <- list(
    clone = function() cloned_chat
  )
  class(mock_chat) <- "Chat"

  withr::local_options(reviewer.client = mock_chat)

  result <- new_reviewer_chat(NULL, "new system prompt")
  expect_equal(result$system_prompt, "new system prompt")
})

test_that("new_reviewer_chat() errors for invalid option type", {
  withr::local_options(reviewer.client = 123)
  expect_snapshot(new_reviewer_chat(NULL, "system prompt"), error = TRUE)
})

test_that("new_reviewer_chat() errors for non-scalar string option", {
  withr::local_options(reviewer.client = c("model1", "model2"))
  expect_snapshot(new_reviewer_chat(NULL, "system prompt"), error = TRUE)
})

# Tests for reviewer.pending_edits option

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
