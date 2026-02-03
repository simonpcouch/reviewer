# Review an R script with an LLM

Launches an interactive Shiny app where an LLM reviews your R code and
suggests improvements for reproducibility, readability, and resilience.
The app displays your code with proposed edits highlighted inline. You
can accept or reject each suggestion, and accepted edits are applied to
the file immediately.

## Usage

``` r
review(file_path, model = "anthropic/claude-sonnet-4-5-20250929")
```

## Arguments

- file_path:

  Path to the R file to review.

- model:

  The model to use for the review, specified as a `"provider/model"`
  string in the same format as
  [`ellmer::chat()`](https://ellmer.tidyverse.org/reference/chat-any.html).
  Defaults to Claude Sonnet 4.5.

## Value

The function's main purpose is its side-effect, a Docs-style interface
opened in the browser. On app close, the
[ellmer::Chat](https://ellmer.tidyverse.org/reference/Chat.html) object
used for the review session is returned (invisibly).

## Examples

``` r
if (FALSE) { # \dontrun{
review("analysis.R")

review("script.R", model = "openai/gpt-5")
} # }
```
