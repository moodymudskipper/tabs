# rstudioapi::documentId(FALSE) is too fast and returns older id so we loop
navigate_to_id <- function(id, info = info_tabs()) {
  current_id <- rstudioapi::documentId(FALSE)
  # if we're already on the right tab there no need to do anything
  if(id == current_id) return(invisible(id))
  path <- info$path[info$id == id]

  rstudioapi::navigateToFile(path)
  repeat {
    new_id <- rstudioapi::documentId(FALSE)
    if(new_id == id) break
  }
  invisible(id)
}

#' @export
abc <- function(..., desc = FALSE) {
  data <- tidyselect::peek_data()
  named_selection <- tidyselect::eval_select(rlang::expr(c(...)), data)
  named_selection[order(names(named_selection), named_selection, decreasing = desc)]
}
