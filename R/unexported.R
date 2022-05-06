untitled_diff <- function(id = NULL, info = info_tabs()) {
  if (is.null(id)) id <- rstudioapi::documentId(FALSE)
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir))
  old <- file.path(dir, "old")
  file.create(old)
  tab_name <- info$tab_name[info$id == id]
  content <- info$cached_contents[[which(info$id == id)]]
  new <- file.path(dir, tab_name)
  writeLines(content, new)
  files <- list(old = old, new = new)
  new_name <- paste(tab_name, "(new)")
  names(files)[2] <- new_name
  tmp_path <- tempfile(fileext=".html")
  print(diffobj::diffFile(
    old, new,
    tar.banner = paste(tab_name, "(saved)"),
    cur.banner = paste(tab_name, "(unsaved)"),
    pager=list(file.path=tmp_path)))
  invisible(tmp_path)
}

unsaved_diff <- function(id = NULL, info = info_tabs()) {
  if (is.null(id)) id <- rstudioapi::documentId(FALSE)
  new <- info$cached_path[info$id == id]
  old <- info$path[info$id == id]
  tab_name <- info$tab_name[info$id == id]
  tmp_path <- tempfile(fileext=".html")
  print(diffobj::diffFile(
    old, new,
    tar.banner = paste(tab_name, "(saved)"),
    cur.banner = paste(tab_name, "(unsaved)"),
    pager=list(file.path=tmp_path),
    mode = "sidebyside"
  ))
  invisible(tmp_path)
}
