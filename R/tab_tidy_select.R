#' @export
files_tidy_select <- function(...) {

  info <- info_files()
  info_split <- split(info, seq_len(nrow(info)))
  # FIXME we want to do the tidy selection on tab names without extension, but
  # not have false positives for "my.data.frame", so viewer tabs should be
  # excluded from this principle

  ind <- tidyselect::eval_select(
    rlang::expr(c(...)),
    rlang::set_names(info_split, info$file_name_no_ext))
  paths <- info$path[ind]
  # maybe we should return a list of rows (simplified to lists)
  # This would avoid redundant calls to info_tabs
  paths
}

#' @export
tabs_tidy_select <- function(...,  info = info_tabs()) {

  info_split <- split(info, seq_len(nrow(info)))

  tab_name_no_ext <- info$tab_name
  has_ext <- !is.na(info$path) & grepl("\\.[^.]+$", tab_name_no_ext)
  tab_name_no_ext[has_ext] <- sub("\\.[^.]+$", "", tab_name_no_ext[has_ext])
  ind <- tidyselect::eval_select(
    rlang::expr(c(...)),
    rlang::set_names(info_split, tab_name_no_ext))
  ids <- info$id[ind]
  # maybe we should return a list of rows (simplified to lists)
  # This would avoid redundant calls to info_tabs
  ids
}
