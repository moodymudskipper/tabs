# FIXME: we can simplify a lot just by naming rows by id in tab_info
#  not sure if we should keep cached, tabs and files together, these might be
#  kept as separate concepts

#TODO: files_tidy_select

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
tabs_tidy_select <- function(..., include_closed = FALSE, include_cached = FALSE, info = info_tabs()) {
  if (!include_cached) {
    info <- info[info$open,]
  }
  if (include_closed) {
    paths <- setdiff(list.files(recursive = TRUE), info$project_path)
    content <- lapply(paths, readLines)
    closed_files <- data.frame(open = FALSE, project_path = paths, saved_contents = I(content), tab_name = basename(paths))
    other_cols <- setdiff(names(info), names(closed_files))
    closed_files[other_cols] <- NA
    info <- rbind(info, closed_files)
  }

  info_split <- split(info, seq_len(nrow(info)))
  # FIXME we want to do the tidy selection on tab names without extension, but
  # not have false positives for "my.data.frame", so viewer tabs should be
  # excluded from this principle

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
