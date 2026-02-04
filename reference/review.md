# Review an R script with an LLM

Launches an interactive Shiny app where an LLM reviews your R code and
suggests improvements for reproducibility, readability, and resilience.
The app displays your code with proposed edits highlighted inline. You
can accept or reject each suggestion, and accepted edits are applied to
the file immediately.

## Usage

``` r
review(file_path, client = NULL, max_pending = NULL)
```

## Arguments

- file_path:

  Path to the R file to review.

- client:

  The model to use for the review, specified as a `"provider/model"`
  string in the same format as
  [`ellmer::chat()`](https://ellmer.tidyverse.org/reference/chat-any.html).
  If not provided, the `reviewer.chat` option must be set. See
  [reviewer_options](https://simonpcouch.github.io/reviewer/reference/reviewer_options.md)
  for details.

- max_pending:

  Maximum number of pending edits allowed at once before the model waits
  for user responses. Higher values reduce wait time but may feel more
  overwhelming and risk edit conflicts. If not provided, the
  `reviewer.pending_edits` option is used. Defaults to 2.

## Value

The function's main purpose is its side-effect, a Docs-style interface
opened in the browser. On app close, the
[ellmer::Chat](https://ellmer.tidyverse.org/reference/Chat.html) object
used for the review session is returned invisibly. The underlying chat
object is stored as `.last_review` on the search path for easy access
after review.

## Examples

``` r
if (FALSE) { # \dontrun{
# Set the chat option in your .Rprofile
options(reviewer.chat = ellmer::chat_claude(model = "claude-sonnet-4-5"))
review("analysis.R")

# Or pass the client directly
review("script.R", client = "openai/gpt-5")
} # }
```
