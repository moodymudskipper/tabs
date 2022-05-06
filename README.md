
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tabs

EXPERIMENTAL

Very rough sketch, many things are likely to change or break.

{tabs} extends {rstudioapi} to be able to conveniently close or move
tabs, show diffs between unsaved and saved, restore tabs from cache etc.

## Installation

Install with:

``` r
remotes::install_github("moodymudskipper/tabs")
```

## How to

`tabs_` functions mostly take a tidy selection as their main arg, which
is applied on tab names. e.g. `tabs_keep(starts_with("a"))`

The most important functions:

-   `tabs_keep()` keeps matched tabs open
-   `tabs_close()` closes matched tabs
-   `tabs_select()` keeps matched tabs and attempts to reorder them
    (There are some limitation for viewed items than cannot be
    recreated)
-   `tabs_gather()` moves matched tabs to the right side of the current
    tab
-   `tabs_open()` opens new tabs by matching file names
-   `tabs_resurrect()` brings back scripts that were closed without
    being saved but still live in the cache (this one doesn’t use tidy
    selection)

By default when closing a tab might cause losing unsaved changeds a diff
is shown and a choice is proposed.

The usual tidy selection helpers can be used to select by tab name or
file name, ignoring the extension.

Other helpers are available :

-   `is_r()`, `is_rmd()` restricts selection to R files or Rmd files
-   `ext_is()` restricts to file that matches the provided extension
    without “.”
-   `code_matches()` restricts selection to files whose code matches the
    provided pattern
-   `code_uses()` restricts selection to R scripts whose code uses the
    provided variable
-   `stored_in()` restricts selection to files located in a given
    directory, optionally recursive
