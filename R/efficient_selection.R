# efficient selection is not a priority for tidyselect
# in case we need it we can use those

and <- function(...) {
  dots <- enquos(...)
  data <- tidyselect::peek_data()
  sel <- seq_along(data)
  for (dot in dots) {
    sel <- sel[tidyselect::eval_select(dot, data[sel])]
  }
  sel
}

or <- function(...) {
  dots <- enquos(...)
  data <- tidyselect::peek_data()
  unsel <- seq_along(data)
  sel <- integer()
  for (dot in dots) {
    unsel <- setdiff(unsel, sel)
    sel <- union(sel, unsel[tidyselect::eval_select(dot, data[unsel])])
  }
  sel
}

# library(dplyr, w = F )
# system.time(
#   select(band_members, and(name, where(~ {Sys.sleep(1); is.character(.)})))
# )
#
# system.time(
#   select(band_members, or(name, where(~ {Sys.sleep(1); is.character(.)})))
# )
#
# system.time(
#   select(iris, and(or(Species, Sepal.Length), !where(~ {Sys.sleep(1); is.numeric(.)})))
# )
