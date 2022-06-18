
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

#' @export
is_view <- function() {
  data <- tidyselect::peek_data()
  fun <- function(row) row$type %in% c("r_data_frame", "object_explorer")
  which(vapply(data, fun, logical(1)))
}

#' @export
is_script <- function() {
  data <- tidyselect::peek_data()
  fun <- function(row) row$is_script
  which(vapply(data, fun, logical(1)))
}

#' @export
code_defines <- function(var, nested = FALSE, cached = FALSE) {
  data <- tidyselect::peek_data()
  sym <- substitute(var)
  fun <- function(row) {
    if (!isTRUE(row$type == "r_source")) return(FALSE)
    code <- if(cached) chr_to_lng(row$cached_contents[[1]]) else chr_to_lng(row$saved_contents[[1]])
    if (!nested) {
      matches_lgl <- sapply(code, function(call)
        is.call(call) && identical(call[[1]], quote(`<-`)) && identical(call[[2]], sym))
      return(any(matches_lgl))
    }
    code_defines_impl(code, sym)
  }
  which(vapply(data, fun, logical(1)))
}

# we don't export this one
has_doc_ids <- function(ids) {
  data <- tidyselect::peek_data()
  match(ids, sapply(data, `[[`, "id"))
}
