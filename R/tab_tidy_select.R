#' @export
files_tidy_select <- function(...) {

  info <- info_files()
  info_split <- split(info, seq_len(nrow(info)))

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
  ind <- tidyselect::eval_select(
    rlang::expr(c(...)),
    rlang::set_names(info_split, info$tab_name_no_ext))
  ids <- info$id[ind]
  # maybe we should return a list of rows (simplified to lists)
  # This would avoid redundant calls to info_tabs
  ids
}
