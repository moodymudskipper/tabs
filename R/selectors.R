#' @export
tabs_close <- function(..., save = NA) {
  info <- info_tabs()
  info_rows <- tabs_tidy_select(..., info = info)
  ids <- names(info_rows)
  for(info_row in info_rows) {
    if(is.na(save) && info_row$dirty) {
      script_is_untitled <- is.na(info_row$path)
      if(script_is_untitled) {
        tmp <- untitled_diff(info_row$id, info)
      } else {
        tmp <- unsaved_diff(info_row$id, info)
      }

      choice <- select.list(c("Save and close", "Close without saving", "Keep as is"), title = "File has unsaved changes:")
      file.remove(tmp)
      switch(
        choice,
        "Keep as is" = next,
        "Close without saving" = rstudioapi::documentClose(info_row$id, save = FALSE),
        "Save and close" = {
          if (script_is_untitled) {
            path <- rstudioapi::selectFile(path = "R", existing = FALSE, caption = "Save as")
            content <- info_row$cached_contents
            writeLines(content, path)
          } else {
            rstudioapi::documentClose(info_row$id, save = FALSE)
          }
        }
      )
      # since we cannot focus on given tab we can show diff in viewer
    } else {
      rstudioapi::documentClose(info_row$id, save = save)
    }
  }
  invisible(info_rows)
}

#' @export
tabs_review <- function(..., save = NA) {
  info <- info_tabs()
  if (...length()) {
    info_rows <- tabs_tidy_select(..., info = info)
  } else {
    info_rows <- tabs_tidy_select(everything(), info = info)
  }
  ids <- names(info_rows)
  for(id in ids) {
    tab_is_view <- info[id, "type"] %in% c("r_dataframe", "object_explorer")
    script_is_saved <-
      !is.na(info[id, "saved_contents"]) &&
      identical(info[id, "saved_contents"], info[id, "cached_contents"])

    tab_name <- info[id, "tab_name"]
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
    script_is_untitled <- is.na(info[id, "path"])
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
          content <- info[id, "cached_contents"]
          writeLines(content, path)
          rstudioapi::documentClose(id, save = FALSE)
        } else {
          rstudioapi::documentClose(id, save = FALSE)
        }
      },
      "Save and keep" = {
        if (script_is_untitled) {
          path <- rstudioapi::selectFile(path = "R", existing = FALSE, caption = "Save as")
          content <- info[id, "cached_contents"]
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
  info_rows <- tabs_tidy_select(..., info = info)
  ids <- names(info_rows)
  for(id in ids) {
    # if saved script, just close and reopen
    path <- info[id, "path"]
    existing_file_is_saved <- !is.na(path) && !info[id, "dirty"]
    if (existing_file_is_saved) {
      rstudioapi::documentClose(id, save = FALSE)
      rstudioapi::navigateToFile(path)
      next
    }

    existing_file_is_unsaved <- !is.na(path) && info[id, "dirty"]
    if (existing_file_is_unsaved) {
      cached_content <- paste(
        info[id, "cached_contents"][[1]],
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

    tab_is_dataframe_viewer <-  info[id, "type"] == "r_dataframe"
    if (tab_is_dataframe_viewer) {
      prop <- info[id, "properties"]
      expr <- prop$expression
      caption <- prop$caption
      env_chr <- prop$environment
      # not sure how "_rs_no_env" happens but trying global env seems like a reasonable heuristic
      env <- if(env_chr %in% c("", "_rs_no_env"))  .GlobalEnv else as.environment(env_chr)
      obj <- try(eval(parse(text=expr)[[1]], env), silent = TRUE)
      if (inherits(obj, "try-error")) {
        rlang::warn(paste0(
          "couldn't move data viewer tab '",
          info[id, "tab_name"],
          "' because the data cannot be recreated"
        ))
        next
      }
      rstudioapi::documentClose(id, save = FALSE)
      View <- get("View", envir = as.environment("package:utils"))
      View(obj, caption)
      next
    }

    tab_is_object_explorer <-  info[id, "type"] == "object_explorer"
    if (tab_is_object_explorer) {
      rlang::warn("Gathering 'object explorer' tabs is not supported")
      next
    }

    tab_is_untitled <- is.na(info[id, "path"])
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

#' @export
tabs_select <- function(..., save_closed = NA) {

  # close unneeded tabs --------------------------------------------------------
  info <- info_tabs()
  info_rows <- tabs_tidy_select(..., info = info)
  # to do : necessary ?
  # info_to_close <- info[!info$id %in% names(info_rows),, drop = FALSE]
  # info_rows_to_close <- split(info_to_close, seq_len(nrow(info_to_close)))
  # tabs_close_impl(info_rows_to_close, save = NA)
  tabs_close(!has_doc_ids(names(info_rows)), save = NA)

  # refresh and order kept tabs ------------------------------------------------
  # note : we refresh because this might be more than initially selected
  info_after_close <- info_tabs()
  info <- info_after_close[union(names(info_rows), info_after_close$id),, drop = FALSE]


  # order titled tabs ----------------------------------------------------------
  # done by navigating to first titled tab and gathering other titled tabs

  # needed because to create a temp file first we need a saved tab to zoom to first
  if (info$is_untitled[[1]]) stop("We don't support selecting an Untitled tab first")
  info_titled <- info[!info$is_untitled,, drop = FALSE]
  rstudioapi::navigateToFile(info_titled$path[1])
  tabs_gather(!!!has_doc_ids(info_titled$id[-1]))

  if(!any(info$is_untitled)) return(invisible(selection))

  repeat {
    info_after_gather <- info_tabs()
    if(nrow(info_after_gather) == nrow(info)) break()
    message("iterating")
  }

  # refresh ids ----------------------------------------------------------------
  # we refreshed titled tabs so their ids changed, but not their paths
  # we fetch order from old info since it's ordered right
  info$order <- seq_len(nrow(info))
  info <- merge(info_after_gather, info[c("order", "path")])
  info <- info[order(info$order), names(info_after_gather)]
  row.names(info) <- info$id

  # compute reference tab and setup loop ---------------------------------------

  ref_tabs <- with(info, id[!is_untitled & is_script])
  info$ref_tab <- with(info, ref_tabs[cumsum(id %in% ref_tabs)])
  untitled_tab_nms <- info[info$is_untitled, "tab_name"]
  n <- max(as.numeric(sub("^Untitled", "", untitled_tab_nms)))

  # remove and recreate unititled tabs -----------------------------------------
  tabs_close(all_of(untitled_tab_nms), save = FALSE)
  for (i in seq_len(n)) {
    tab_nm <- paste0("Untitled", i)
    if(!tab_nm %in% untitled_tab_nms) {
      # create a tab just to close it in the end
      rstudioapi::documentNew("")
      next
    }
    row_ind <- info$tab_name == tab_nm
    focus_titled_tab_id <- info[row_ind, "ref_tab"]
    navigate_to_id(focus_titled_tab_id, info)
    contents <- info[row_ind, "cached_contents"][[1]]

    rstudioapi::documentNew(contents)
  }
  to_close <- setdiff(paste0("Untitled", seq_len(n)), untitled_tab_nms)
  tabs_close(all_of(to_close), save = FALSE)
  invisible(info_tabs())
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

#' @export
tabs_goto <- function(selection) {
  info_rows <- tabs_tidy_select({{selection}})
  info_rows <- Filter(function(row) !row$is_untitled, info_rows)
  if (!length(info_rows)) rlang::inform("The selection didn't match any open tab")
  id <- sort(names(info_rows))[[1]]
  navigate_to_id(id)
}

#' @export
tabs_delete <- function(..., review = TRUE, scope = c("tabs", "files")) {
  scope <- match.arg(scope)
  if (scope == "files") {
    paths <- files_tidy_select(...)
    for (path in paths) {
      rstudioapi::navigateToFile(path)
      if (review) {
        Sys.sleep(.1)
        rstudioapi::sendToConsole("", FALSE)
        choice <- select.list(c("Remove", "Keep"))
        rstudioapi::documentClose()
        if (choice == "Remove") file.remove(path)
      } else {
        file.remove(path)
      }
    }
  } else {
    info_rows <- tabs_tidy_select(...)
    if (!length(info_rows)) {
      path <- rstudioapi::documentPath()
      if (is.null(path)) rlang::abort("This tab cannot be deleted")
      info_rows <- list(rows = list(path = path))
    }
    for (row in info_rows) {
      rstudioapi::navigateToFile(row$path)
      if (review) {
        Sys.sleep(.1)
        rstudioapi::sendToConsole("", FALSE)
        choice <- select.list(
          title = sprintf("Remove '%s' ?", row$path),
          c("Remove", "Keep"))
        if (choice == "Remove") {
          rstudioapi::documentClose()
          file.remove(row$path)
        }
      } else {
        rstudioapi::documentClose()
        file.remove(row$path)
      }
    }
  }
}

#' @export
tabs_rename <- function(name, overwrite = FALSE) {
  name <- as.character(substitute(name))
  rstudioapi::documentSave()
  old_path <- rstudioapi::documentPath()
  old_name <- basename(old_path)
  ext <- tools::file_ext(old_name)
  dir <- dirname(old_path)
  if (ext == "") {
    path <- sprintf("%s/%s", dir, name)
  } else {
    path <- sprintf("%s/%s.%s", dir, name, ext)
  }
  success <- file.copy(old_path, path, overwrite = overwrite)
  if (!success) {
    rlang::abort(c(
      "Copy failed",
      x = sprintf("Destination path '%s' already exists", path),
      i = "Do you need `overwrite = TRUE`?"
    ))
  }
  old_id <- rstudioapi::documentId(allowConsole = FALSE)
  rstudioapi::navigateToFile(path)
  rstudioapi::documentClose(old_id)
  file.remove(old_path)
}

#' @export
tabs_path <- function(...) {
  if(!...length()) return(rstudioapi::documentPath())
  info <- info_tabs()
  info_rows <- tabs_tidy_select(..., info = info)
  unname(sapply(info_rows, `[[`, "path"))
}
