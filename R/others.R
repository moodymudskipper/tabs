# sandbox, some obsolete functions and other rough drafts


# FIXME: move to ghstudio
# get_git_log <- function(...) {
#   log_char <- system('git log --pretty=format:"%h;;%an;;%ad;;%s"', intern = TRUE)
#   log_mat <- do.call(rbind, strsplit(log_char, ";;"))
#   colnames(log_mat) <- c("hash", "author", "date", "title")
#   dplyr::filter(as.data.frame(log_mat), ...)
# }

#' Print diff between active document and saved version
#'
#' @inheritParams diffobj::diffFile
#' @param ... Additional parameters passed to `diffobj::diffFile`
#'
#' @return Returns `NULL` invisibly, called for side effects
#' @export
document_diff <- function(
    id = rstudioapi::documentId(FALSE),
    mode = c("auto", "unified", "sidebyside", "context"),
    context = 2,
    ...) {
  mode <- match.arg(mode)
  id0 <- id
  row <- subset(info_tabs(), id == id0)
  if (!is.na(row$path))
    print(diffobj::diffFile(row$path, row$cached_path, mode, context, ...))
  invisible(NULL)
}

