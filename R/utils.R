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
  named_selection <- eval_select2(rlang::expr(c(...)), data)
  named_selection[order(names(named_selection), named_selection, decreasing = desc)]
}


chr_to_lng <- function(code) {
  if(!length(code)) return(NULL)
  if (length(code) == 1) return (str2lang(as.character(code)))
  as.call(c(quote(`{`), parse(text = code)))
}


code_defines_impl <- function(call, sym) {
  if (!is.call(call)) return(FALSE)
  if (identical(call[[1]], quote(`<-`)) && identical(call[[2]], sym)) return(TRUE)
  any(sapply(call, code_defines_impl, sym))
}
