
#' @export
code_uses <- function(var) {
  data <- tidyselect::peek_data()
  fun <- function(row)
    !is.na(row$path) &&
    !is.na(row$type) &&
    row$type == "r_source" &&
    var %in% all.names(parse(file = print(row$path)), unique = TRUE)
  which(vapply(data, fun, logical(1)))
}

#' @export
code_matches <- function(pattern) {
  data <- tidyselect::peek_data()
  fun <- function(row)
    !is.na(row$path) &&
    grepl(pattern, paste(readLines(row$path), collapse = "\n"), ...)
  which(vapply(data, fun, logical(1)))
}

#' @export
is_r <- function() {
  data <- tidyselect::peek_data()
  fun <- function(row) !is.na(row$type) && row$type == "r_source"
  which(vapply(data, fun, logical(1)))
}

#' @export
is_rmd <- function() {
  data <- tidyselect::peek_data()
  fun <- function(row) !is.na(row$type) && row$type == "r_markdown"
  which(vapply(data, fun, logical(1)))
}

#' @export
ext_is <- function(ext, ignore.case = TRUE) {
  data <- tidyselect::peek_data()
  fun <- function(row) {
    if(is.na(row$path)) return(FALSE)
    if (ignore.case) {
      endsWith(tolower(row$path), paste0(".", tolower(ext)))
    } else {
      endsWith(row$path, paste0(".", tolower(ext)))
    }
  }
  which(vapply(data, fun, logical(1)))
}

#' @export
stored_in <- function(dir, recursive = TRUE) {
  data <- tidyselect::peek_data()
  fun <-   function(row) {
    !is.na(row$path) && normalizePath(row$path) %in% normalizePath(list.files(path = dir, recursive = recursive, full.names = TRUE))
  }
  which(vapply(data, fun, logical(1)))
}

#' @export
is_saved <- function() {
  data <- tidyselect::peek_data()
  fun <- function(row) identical(row$saved_contents, row$cached_contents)
  which(vapply(data, fun, logical(1)))
}
