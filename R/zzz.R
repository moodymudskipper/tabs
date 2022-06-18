#' @importFrom utils select.list
#' @importFrom rlang %||%
NULL

globalVariables(c("meta_data_dir", "everything", "View"))

meta_data_dir <- NULL

.onLoad <- function(lib, pkg){

  options(tabs.max_tabs = getOption("tabs.max_tabs") %||% 10)

  if (!rstudioapi::isAvailable()) return()
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

  meta_data_dir <<- s_dir
}
