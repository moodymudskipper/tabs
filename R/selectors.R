#' @export
tabs_close <- function(..., save = NA) {
  info <- info_tabs()
  ids <- tabs_tidy_select(..., info = info, include_closed = FALSE)
  for(id in ids) {
    if(is.na(save) && info$dirty[info$id == id]) {
      script_is_untitled <- is.na(info$path[info$id == id])
      if(script_is_untitled) {
        tmp <- untitled_diff(id, info)
      } else {
        tmp <- unsaved_diff(id, info)
      }

      choice <- select.list(c("Save and close", "Close without saving", "Keep as is"), title = "File has unsaved changes:")
      file.remove(tmp)
      switch(
        choice,
        "Keep as is" = next,
        "Close without saving" = rstudioapi::documentClose(id, save = FALSE),
        "Save and close" = {
          if (script_is_untitled) {
            path <- rstudioapi::selectFile(path = "R", existing = FALSE, caption = "Save as")
            content <- info$cached_contents[[which(info$id == id)]]
            writeLines(content, path)
          } else {
            rstudioapi::documentClose(id, save = FALSE)
          }
        }
      )
      # since we cannot focus on given tab we can show diff in viewer
    } else {
      rstudioapi::documentClose(id, save = save)
    }
  }
  invisible(ids)
}

#' @export
tabs_review <- function(..., save = NA) {
  info <- info_tabs()
  ids <- tabs_tidy_select(..., info = info, include_closed = FALSE)
  for(id in ids) {
    tab_is_view <- info$type[info$id == id] %in% c("r_dataframe", "object_explorer")
    script_is_saved <-
      !is.na(info$saved_content[info$id == id]) &&
      identical(info$saved_content[info$id == id], info$cached_contents[info$id == id])

    tab_name <- info$tab_name[info$id == id]
    if (tab_is_view || script_is_saved) {
      if (tab_is_view)
        title <- sprintf("View '%s':", tab_name)
      else
        title <- sprintf("Script '%s':", tab_name)
      choice <- select.list(c("Close",  "Keep"), title = title)
      switch(
        choice,
        "Keep" = next,
        "Close" = rstudioapi::documentClose(id, save = FALSE)
      )
      next
    }
    script_is_untitled <- is.na(info$path[info$id == id])
    if(script_is_untitled) {
      tmp <- untitled_diff(id, info)
    } else {
      tmp <- unsaved_diff(id, info)
    }
    choice <- select.list(c("Save and close", "Close without saving", "Keep as is", "Save and keep"), sprintf("Script '%s':", tab_name))
    file.remove(tmp)
    switch(
      choice,
      "Keep as is" = next,
      "Close without saving" = rstudioapi::documentClose(id, save = FALSE),
      "Save and close" = {
        if (script_is_untitled) {
          path <- rstudioapi::selectFile(path = "R", existing = FALSE, caption = "Save as")
          content <- info$cached_contents[[which(info$id == id)]]
          writeLines(content, path)
          rstudioapi::documentClose(id, save = FALSE)
        } else {
          rstudioapi::documentClose(id, save = FALSE)
        }
      },
      "Save and keep" = {
        if (script_is_untitled) {
          path <- rstudioapi::selectFile(path = "R", existing = FALSE, caption = "Save as")
          content <- info$cached_contents[[which(info$id == id)]]
          writeLines(content, path)
        } else {
          rstudioapi::documentSave(id)
        }
      },
    )
  }
  invisible(ids)
}

#' @export
tabs_gather <- function(...) {
  info <- info_tabs()
  ids <- tabs_tidy_select(..., info = info, include_closed = FALSE)
  for(id in ids) {
    # if saved script, just close and reopen
    path <- info$path[info$id == id]
    existing_file_is_saved <- !is.na(path) && !info$dirty[info$id == id]
    if (existing_file_is_saved) {
      rstudioapi::documentClose(id, save = FALSE)
      rstudioapi::navigateToFile(path)
      next
    }

    existing_file_is_unsaved <- !is.na(path) && info$dirty[info$id == id]
    if (existing_file_is_unsaved) {
      cached_content <- paste(
        info$cached_contents[[which(info$id == id)]],
        collapse = "\n")
      rstudioapi::documentClose(id, save = FALSE)

      focused_id <- rstudioapi::documentId(FALSE)
      rstudioapi::navigateToFile(path)
      # rstudioapi::documentId(FALSE) is too fast and returns older id so we loop
      repeat {
        new_id <- rstudioapi::documentId(FALSE)
        if(new_id != focused_id) break
      }
      rstudioapi::setDocumentContents(cached_content, new_id)
      next
    }

    tab_is_dataframe_viewer <-  info$type[info$id == id] == "r_dataframe"
    if (tab_is_dataframe_viewer) {
      prop <- info$properties[[which(info$id == id)]]
      expr <- prop$expression
      caption <- prop$caption
      env_chr <- prop$environment
      # not sure how "_rs_no_env" happens but trying global env seems like a reasonable heuristic
      env <- if(env_chr %in% c("", "_rs_no_env"))  .GlobalEnv else as.environment(env_chr)
      obj <- try(eval(parse(text=expr)[[1]], env), silent = TRUE)
      if (inherits(obj, "try-error")) {
        rlang::warn(paste0(
          "couldn't move data viewer tab '",
          info$tab_name[info$id == id],
          "' because the data cannot be recreated"
        ))
        next
      }
      rstudioapi::documentClose(id, save = FALSE)
      View <- get("View", envir = as.environment("package:utils"))
      View(obj, caption)
      next
    }

    tab_is_object_explorer <-  info$type[info$id == id] == "object_explorer"
    if (tab_is_object_explorer) {
      rlang::warn("Gathering 'object explorer' tabs is not supported")
      next
    }

    tab_is_untitled <- is.na(info$path[info$id == id])
    if (tab_is_untitled) {
      rlang::warn("Gathering untitled tabs is not supported")
      next
    }
    rlang::warn("Moving this type of tab is currently unsupported")
  }
}

# remove all tabs that are not selected

#' @export
tabs_keep <- function(..., save_closed = NA) {
  tabs_close(!c(...), save = save_closed)
}

# FIXME : This code is horrendous, it uses tidy selection 3 times, expensive and unnecessary
#   better use the tidy selection only once and when ids are reassigned match them
#   by tab names using the info table. Should already look much better by using
#   row indexing in info
#

#' @export
tabs_select <- function(..., save_closed = NA) {
  tabs_keep(..., save_closed = save_closed)

  info <- info_tabs()
  selection <- tabs_tidy_select(...)

  #other_selection <- selection[-1]
  ind <- match(selection, info$id)

  # order titled
  untitled_selection_lgl <- info$is_untitled[ind]
  titled_selection <- selection[!untitled_selection_lgl]
  titled_tab_nms <- info$tab_name_no_ext[match(titled_selection, info$id)]

  path1 <- info$path[match(selection[1], info$id)]
  rstudioapi::navigateToFile(path1)
  tabs_gather(!!!titled_tab_nms[-1])

  #make sure gathering is over before fetching new info, we might make this more robust
  Sys.sleep(.05)

  # refresh info, we recreated some tabs so ids changed
  info <- info_tabs()
  selection <- tabs_tidy_select(..., info = info)

  if(!any(untitled_selection_lgl)) return(invisible(selection))
  # order untitled

  ind <- match(selection, info$id)
  untitled_selection_lgl <- info$is_untitled[ind]
  script_selection_lgl <- info$is_script[ind]
  untitled_selection <- selection[untitled_selection_lgl]
  titled_selection <- selection[!untitled_selection_lgl]
  viewer_selection <- selection[!script_selection_lgl]


  # the reference tabs are titled scripts (no untitled, no viewer)
  ref_tabs <- setdiff(titled_selection, viewer_selection)[cumsum(!selection %in% c(untitled_selection, viewer_selection))]
  names(selection) <- ref_tabs
  untitled_selection <- selection[untitled_selection_lgl]
  untitled_tab_nms <- info$tab_name_no_ext[match(untitled_selection, info$id)]
  untitled_selection <- untitled_selection[order(untitled_tab_nms)]
  n <- max(as.numeric(sub("^Untitled", "", untitled_tab_nms)))

  contents <- info$cached_contents[match(untitled_selection, info$id)]
  tabs_close(all_of(untitled_tab_nms), save = FALSE)

  j <- 0
  for (i in seq_len(n)) {
    tab_nm <- paste0("Untitled", i)
    if(!tab_nm %in% untitled_tab_nms) {
      # create a tab just to close it in the end
      rstudioapi::documentNew("")
      next
    }
    j <- j + 1
    focus_titled_tab_id <- names(untitled_selection)[j]
    navigate_to_id(focus_titled_tab_id, info)
    rstudioapi::documentNew(contents[j])
  }
  to_close <- setdiff(paste0("Untitled", seq_len(n)), untitled_tab_nms)
  tabs_close(all_of(to_close), save = FALSE)
  invisible(selection)
}

#' @export
tabs_resurrect <- function(id = NULL) {
  info <- info_chached()
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

#' @export
tabs_open <- function(...) {
  paths <- files_tidy_select(...)
  for (path in paths) {
    rstudioapi::navigateToFile(path)
  }
  invisible(paths)
}
