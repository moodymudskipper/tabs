
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tabs

EXPERIMENTAL

Very rough sketch, many things are likely to change or break.

On github for the issue tracker mainly.

{tabs} extends {rstudioapi} to be able to conveniently close or move
tabs, show diffs between unsaved and saved, restore tabs from cache etc.

It’s been developped because it’s needed for {tricks} but has general
features that fit better in a separate package.

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
-   `tabs_resurrect()` brings back scripts that were closed without
    being saved but still live in the cache (this one doesn’t use tidy
    selection)
