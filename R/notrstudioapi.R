# FIXME: move to ghstudio
get_git_log <- function(...) {
  log_char <- system('git log --pretty=format:"%h;;%an;;%ad;;%s"', intern = TRUE)
  log_mat <- do.call(rbind, strsplit(log_char, ";;"))
  colnames(log_mat) <- c("hash", "author", "date", "title")
  dplyr::filter(as.data.frame(log_mat), ...)
}

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
  row <- subset(documents_info(), id == id0)
  if (!is.na(row$path))
    diffobj::diffFile(row$path, row$cached_path, mode, context, ...)
  invisible(NULL)
}

#' @export
documents_diff <- function(
  untitled = FALSE,
  mode = c("auto", "unified", "sidebyside", "context"),
  context = 2,
  ...) {
  mode <- match.arg(mode)
  info <- documents_info()
  info <- info[!is.na(info$dirty) & info$open & info$dirty,, drop = FALSE]
  if (!untitled) info <- info[!is.na(info$path) ,, drop = FALSE]
  for (id in info$id) {
    row <- info[info$id == id,, drop = FALSE]
    unsaved <- row$cached_path
    call <- substitute(
      diffobj::diffFile(PATH, unsaved, mode, context, ...),
      list(PATH = row$path))
    print(eval(call))
  }
  invisible(NULL)
}


#' Fetch document info
#'
#' This assumes a folder structure in the `.RProj.user` subfolder (invisible
#'   from RStudio), this folder structure is undocumented as far as I know
#'   so it might change without notice, in this case the package will break.
#'
#' @return
#' @export
#'
#' @examples
documents_info <- function() {
  # The Folder structure is not documented AFAIK so we're making assumption on how it "should" be
  # This is based on limited experience so might break on different contexts, systems, versions...

  # check .Rproj.User/ ---------------------------------------------------------

  r_proj_user_dirs <- list.dirs(".Rproj.User", recursive = FALSE, full.names = FALSE)

  # We expected to have 2 folders, "shared" and a folder "<ID1>" formatted like "87BFFF1D"
  r_proj_user_dirs_as_expected <-
    length(r_proj_user_dirs) == 2 &&
    "shared" %in% r_proj_user_dirs &&
    grepl("^[0-9A-Z]+$", setdiff(r_proj_user_dirs, "shared"))


  if (!r_proj_user_dirs_as_expected) {
    rlang::abort(c(
      "Unexpected folders found in .RProj.User",
      i = "We should find 2 folders : A folder named `shared` and a folder containing numbers and upper case letters",
      x = paste0("Found: ", toString(r_proj_user_dirs))
    ))
  }

  # check .Rproj.User/<ID1>/ ---------------------------------------------------

  sources_dir <- file.path(".Rproj.User",  r_proj_user_dirs[1], "sources")
  if (!dir.exists(sources_dir)) {
    rlang::abort(c(
      paste0("'sources' subfolder was not found in '", file.path(".Rproj.User",  r_proj_user_dirs[1]), "'")
    ))
  }

  # check .Rproj.User/<ID1>/sources/ -------------------------------------------

  s_dir <- grep("^s-[0-9A-Z]+$", list.dirs(sources_dir, recursive = FALSE, full.names = FALSE), value = TRUE)
  if(!length(s_dir) == 1) {
    rlang::abort()
  }
  s_dir <- file.path(".Rproj.User",  r_proj_user_dirs[1], "sources", s_dir)
  files <- list.files(s_dir, full.names = TRUE)

  # fetch info from .Rproj.User/<ID1>/sources/s-<ID2>/ -------------------------
  open_tabs_files <- list.files(s_dir, pattern = "^[0-9A-Z]+$", full.names = TRUE)
  content_files <- list.files(s_dir, pattern = "^[0-9A-Z]+-contents$", full.names = TRUE)
  closed_content_files <- setdiff(content_files, paste0(open_tabs_files, "-contents"))

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
    doc
  })
  res_df <- as.data.frame(do.call(rbind, res))
  res_df[] <- lapply(res_df, function(x) if (all(lengths(x) == 1)) unlist(x) else x)
  res_df$created <- as.POSIXct(res_df$created, origin = "1970-01-01 UTC")
  res_df$lastKnownWriteTime <- as.POSIXct(res_df$lastKnownWriteTime, origin = "1970-01-01 UTC")
  res_df$last_content_update <- as.POSIXct(res_df$last_content_update, origin = "1970-01-01 UTC")
  first_cols <- c("open", "id", "path", "created", "lastKnownWriteTime", "last_content_update", "type", "relative_order")
  other_cols <- setdiff(names(res_df), first_cols)
  res_df <- res_df[,c(first_cols, other_cols)]

  # CLOSED FILES
  if (!length(closed_content_files)) {
    rownames(res_df) <- NULL
    return(res_df)
  }

  res_closed_df <- data.frame(
    open = FALSE,
    id = sub("-content", "", basename(closed_content_files)),
    lastKnownWriteTime = as.POSIXct(sapply(closed_content_files, file.mtime), origin = "1970-01-01 UTC"),
    cached_path = closed_content_files
  )
  res_closed_df$cached_contents <- lapply(closed_content_files, readLines, warn = FALSE)
  res_closed_df$properties <- vector("list", length(closed_content_files))
  res_closed_df$read_only_alternatives <- vector("list", length(closed_content_files))
  other_cols <- setdiff(names(res_df), names(res_closed_df))
  res_closed_df[other_cols] <- NA


  # COMBINE
  res_df <- rbind(res_df, res_closed_df)
  rownames(res_df) <- NULL
  res_df
}


#' Fetch document ids
#'
#' Fetch the ids of open documents with optional filters and order
#'
#' @param type character vector containing types to consider, names are those used
#'   by Rstudio, the list is might not be exhaustive so values out of these are allowed (but
#'   please do open an issue if you find one).
#' @param untitled Whether to consider untitled documents. If `TRUE` only untitled documents
#'   are considered, if `FALSE` only saved documents are considered By default (`NA`) all are considered.
#'
#'
#' @return
#' @export
#'
#' @examples
document_ids <- function(
  type = c("text", "r_source", "r_markdown", "r_namespace", "r_history", "markdown", "gitignore", "yaml", "dcf", "python"),
  untitled = NA,
  name = NULL,
  name_matches = NULL,
  created_after = NULL,
  created_before = NULL,
  saved_after = NULL,
  saved_before = NULL,
  updated_after = NULL,
  updated_before = NULL,
  order = c(
    "name", "name_desc",
    "created", "created_desc",
    "saved", "saved_desc",
    "updated", "updated_desc",
    "untitled_first", "untitled_last",
    "rmd_first", "rmd_last",
    "r_first", "r_last")
) {
  # OR
  # we provide a data frame that can be sorted and filtered, incl resurrecting cached
  # more flexible but less readable


  tabs <- documents_info()
  # type -----------------------------------------------------------------------
  type <- match.arg(any, several.ok = TRUE)
  tabs <- Filter(function(x) x$type %in% type, tabs)

  # untitled -------------------------------------------------------------------
  if (!is.na(untitled)) {
    if(untitled) {
      tabs <- Filter(function(x) is.null(x$path), tabs)
    } else {
      tabs <- Filter(function(x) !is.null(x$path), tabs)
    }
  }

  # untitled -------------------------------------------------------------------
  if (!is.na(untitled)) {
    if(untitled) {
      tabs <- Filter(function(x) is.null(x$path), tabs)
    } else {
      tabs <- Filter(function(x) !is.null(x$path), tabs)
    }
  }
}


#' @export
documents_close_viewers <- function(pattern = NULL) {
  info <- documents_info()
  info <- info[info$type %in% "r_dataframe",, drop = FALSE]
  if (!is.null(pattern)) info <-
    info[sapply(info$properties, function(x) grepl(pattern, x$caption)),, drop = FALSE]
  for (id in info$id) rstudioapi::documentClose(id)
  invisible(NULL)
}

# rstudio::api offers noway to sort tabs but we can just close and reopen even temp files
# by either using the cache or saving content
documents_close <- function(save = NA, ...) {
  # should wrap documents_sort
  # NA means we prompt

  # ... passed to document_ids
  # if order is given, warn that it's ignored
}

documents_save <- function(...) {
  # ... passed to document_ids
  # if order is given, warn that it's ignored
}

documents_untitled_first<- function() {
  info <- documents_info()
  info <- info[info$open,, drop = FALSE]
  View(info)
  rstudioapi::documentClose()
}

documents_sort <- function(save_before_close = NA, ...) {
  # ... passed to document_ids
  # if order is given, warn that it's ignored
}

documents_resurrect <- function(id = NULL) {
  info <- documents_info()
  info <- info[!info$open,, drop = FALSE]
  if (!is.null(id)) info <- info[info$id %in% id,, drop = FALSE]
  mapply(
    info$cached_path,
    info$cached_contents,
    FUN = function(file, contents) {
      if (!length(contents) || startsWith(contents[[1]], "# retrieved from")) return()
      contents <- c(paste("# retrieved from", normalizePath(file)), contents)
      rstudioapi::documentNew(contents)
    })
  invisible(NULL)
}

# we could also have a shiny app to reorganize and resurrect document
