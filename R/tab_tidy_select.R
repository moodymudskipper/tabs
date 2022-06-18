#' @export
files_tidy_select <- function(...) {

  info <- info_files()
  info_rows <- split(info, seq_len(nrow(info)))

  ind <- eval_select2(
    rlang::expr(c(...)),
    rlang::set_names(info_rows, info$file_name_no_ext))
  paths <- info$path[ind]
  # maybe we should return a list of rows (simplified to lists)
  # This would avoid redundant calls to info_tabs
  paths
}

#' @export
tabs_tidy_select <- function(...,  info = info_tabs()) {
  info_rows <- split(info, seq_len(nrow(info)))
  ind <- eval_select2(
    rlang::expr(c(...)),
    rlang::set_names(info_rows, info$tab_name_no_ext))
  rlang::set_names(info_rows[ind], info$id[ind])
}
