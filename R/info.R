# FIXME: crashes when a file was deleted but is still open because readLines is called on a non existing path.
#  solution : check if a column gives this info, else add it with `file.exists`
#  then check if files that don't exist anymore are movable or not (probably not)
#  add tabs_close_deleted() helper
#  in fact we need a collection of tabs_close_ and tabs_review_ helpers
#  tabs_review_untitled is especially useful

# Fetch document info
#
# This assumes a folder structure in the `.RProj.user` subfolder (invisible
#   from RStudio), this folder structure is undocumented as far as I know
#   so it might change without notice, in this case the package will break.
info_tabs <- function() {

  # fetch info from .Rproj.User/<???>/sources/s-<???>/ -------------------------
  open_tabs_files <- list.files(meta_data_dir, pattern = "^[0-9A-Z]+$", full.names = TRUE)

  # OPEN FILES
  names(open_tabs_files) <- basename(open_tabs_files)
  res <- lapply(open_tabs_files, function(x) {
    doc <- jsonlite::read_json(x)
    doc$path <- if (is.null(doc$path)) NA else doc$path
    doc$project_path <- if (is.null(doc$project_path)) NA else doc$project_path
    # `created` and `last_content_update` must be divided by 1000 to make sense, reason unknown
    doc$created <- as.POSIXct(doc$created/1000, origin = "1970-01-01 UTC")
    doc$lastKnownWriteTime <- as.POSIXct(doc$lastKnownWriteTime, origin = "1970-01-01 UTC")
    doc$last_content_update <- as.POSIXct(doc$last_content_update/1000, origin = "1970-01-01 UTC")
    doc$cached_path <- paste0(x, "-contents")
    doc$cached_contents <- readLines(doc$cached_path, warn = FALSE)
    doc$saved_contents <- if (is.na(doc$path)) NA else readLines(doc$path)
    doc$open <- TRUE
    doc$temp_name <- if (is.null(doc$properties$tempName)) NA else doc$properties$tempName
    doc$caption <- if (is.null(doc$properties$caption)) NA else doc$properties$caption
    doc$title <- if (is.null(doc$properties$title)) NA else doc$properties$title
    doc$context <- if (is.null(doc$properties$context)) NA else doc$properties$context
    doc
  })
  info <- as.data.frame(do.call(rbind, res))
  info[] <- lapply(info, function(x) if (all(lengths(x) == 1)) unlist(x) else x)
  info$created <- as.POSIXct(info$created, origin = "1970-01-01 UTC")
  info$lastKnownWriteTime <- as.POSIXct(info$lastKnownWriteTime, origin = "1970-01-01 UTC")
  info$last_content_update <- as.POSIXct(info$last_content_update, origin = "1970-01-01 UTC")

  # add useful cols
  info$is_script <- !info$type %in% c("r_dataframe", "object_explorer")
  info$is_open_script <- info$open & info$is_script
  info$is_untitled <- info$is_script & is.na(info$path)
  info$tab_name <- with(
    info,
    ifelse(!is.na(info$path), basename(info$path),
           ifelse(!is.na(temp_name), temp_name,
                  ifelse(!is.na(caption), caption,
                         ifelse(!is.na(context), sub(".*:::(.+)$", "\\1", context),
                                ifelse(!is.na(title), title ,NA))))))

  # build names without extensions
  has_ext <-  info$is_open_script & grepl("\\.[^.]+$", info$tab_name)
  info$tab_name_no_ext <- info$tab_name
  info$tab_name_no_ext[has_ext] <- sub("\\.[^.]+$", "", info$tab_name_no_ext[has_ext])

  # remove unuseful cols
  cols_to_remove <- c(
    "hash", "contents", "source_on_save", "folds", "collab_server",
    "source_window", "read_only", "read_only_alternatives",
    # added by us
    "temp_name", "caption", "title", "context")
  info[cols_to_remove] <- NULL

  # reorder
  first_cols <- c("tab_name", "id", "open", "is_script", "is_untitled", "type", "path", "created", "lastKnownWriteTime", "last_content_update", "relative_order")
  other_cols <- setdiff(names(info), first_cols)
  info <- info[,c(first_cols, other_cols)]

  row.names(info) <- info$id
  info
}

info_cached <- function() {
  # fetch info from .Rproj.User/<???>/sources/s-<???>/ -------------------------
  open_tabs_files <- list.files(meta_data_dir, pattern = "^[0-9A-Z]+$", full.names = TRUE)
  content_files <- list.files(meta_data_dir, pattern = "^[0-9A-Z]+-contents$", full.names = TRUE)
  closed_content_files <- setdiff(content_files, paste0(open_tabs_files, "-contents"))

  info <- data.frame(
    open = FALSE,
    id = sub("-content", "", basename(closed_content_files)),
    lastKnownWriteTime = as.POSIXct(sapply(closed_content_files, file.mtime), origin = "1970-01-01 UTC"),
    cached_path = closed_content_files
  )
  info$cached_contents <- lapply(closed_content_files, readLines, warn = FALSE)
  # info$properties <- vector("list", length(closed_content_files))
  # info$is_untitled <- FALSE
  # info$read_only_alternatives <- vector("list", length(closed_content_files))
  # other_cols <- setdiff(names(info), names(info))
  # info[other_cols] <- NA
  row.names(info) <- info$id
  info
}



info_files <- function() {
  # we skip the  `all.files = TRUE` to avoid the .git and .RProj dir, and maybe more
  project_paths <- list.files(recursive = TRUE, full.names = TRUE)
  # FIXME : open should be defined after looking at tab info, and cached_contents adapted in consequence
  info <- data.frame(open = FALSE, path = normalizePath(project_paths), project_path = project_paths, file_name = basename(project_paths))
  file_name_no_ext <- info$file_name
  has_ext <- !is.na(info$path) & grepl("\\.[^.]+$", file_name_no_ext)
  file_name_no_ext[has_ext] <- sub("\\.[^.]+$", "", file_name_no_ext[has_ext])
  ext <- ifelse(has_ext, sub("^.*\\.([^.]+)$", "\\1", info$path), "")
  info$file_name_no_ext <- file_name_no_ext
  info$ext <- ext

  types <- c("r_source", "text", "r_html", "r_html", "js", "cpp", "cpp",  "stan",
             "sql", "sweave", "python", "r_doc", "r_markdown", "markdown",
             "gitignore", "r_history", "buildignore", "r_project")
  exts <-  c("r"        ,     "",    "htm",   "html", "js", "cpp",  "cc", "stan",
             "sql",    "rnw", "py"    , "rd"   , "rmd",        "md",
             "gitignore", "rhistory", "text",         "rproj")
  info$type <- types[match(tolower(ext), exts)]
  info$type[info$file_name == "DESCRIPTION"] <- "dcf"
  info$type[info$file_name == "NAMESPACE"] <- "r_namespace"
  info$saved_contents <- NA
  info$saved_contents[!is.na(info$type)] <- lapply(project_paths[!is.na(info$type)], readLines)
  row.names(info) <- info$project_path
  info
}





