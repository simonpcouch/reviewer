test_that("get_reviewer_chat() returns NULL when option not set", {
  withr::local_options(reviewer.chat = NULL)
  expect_null(get_reviewer_chat())
})

test_that("get_reviewer_chat() returns the option value", {
  withr::local_options(reviewer.chat = "test-model")
  expect_equal(get_reviewer_chat(), "test-model")
})

test_that("new_reviewer_chat() errors when no option or model provided", {
  withr::local_options(reviewer.chat = NULL)
  expect_snapshot(new_reviewer_chat(NULL, "system prompt"), error = TRUE)
})

test_that("new_reviewer_chat() uses model argument when option not set", {
  withr::local_options(reviewer.chat = NULL)
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

test_that("new_reviewer_chat() uses string option over model argument",
{
  withr::local_options(reviewer.chat = "openai/gpt-5")
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

  withr::local_options(reviewer.chat = mock_chat)

  result <- new_reviewer_chat(NULL, "new system prompt")
  expect_equal(result$system_prompt, "new system prompt")
})

test_that("new_reviewer_chat() errors for invalid option type", {
  withr::local_options(reviewer.chat = 123)
  expect_snapshot(new_reviewer_chat(NULL, "system prompt"), error = TRUE)
})

test_that("new_reviewer_chat() errors for non-scalar string option", {
 withr::local_options(reviewer.chat = c("model1", "model2"))
  expect_snapshot(new_reviewer_chat(NULL, "system prompt"), error = TRUE)
})
