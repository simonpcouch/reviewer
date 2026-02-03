# get_reviewer_pending_edits() errors for non-numeric option

    Code
      get_reviewer_pending_edits(NULL)
    Condition
      Error in `get_reviewer_pending_edits()`:
      ! The option `reviewer.pending_edits` must be a positive integer, not a string.
      i Set e.g. `options(reviewer.pending_edits = 5)` in your '~/.Rprofile'.

# get_reviewer_pending_edits() errors for non-scalar option

    Code
      get_reviewer_pending_edits(NULL)
    Condition
      Error in `get_reviewer_pending_edits()`:
      ! The option `reviewer.pending_edits` must be a positive integer, not a double vector.
      i Set e.g. `options(reviewer.pending_edits = 5)` in your '~/.Rprofile'.

# get_reviewer_pending_edits() errors for non-positive option

    Code
      get_reviewer_pending_edits(NULL)
    Condition
      Error in `get_reviewer_pending_edits()`:
      ! The option `reviewer.pending_edits` must be a positive integer, not a number.
      i Set e.g. `options(reviewer.pending_edits = 5)` in your '~/.Rprofile'.

