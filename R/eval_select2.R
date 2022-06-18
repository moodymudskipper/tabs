

# tidyselect::eval_select() wrapper that
eval_select2 <- function(expr, data) {
  call <- sys.call(-1)
  rlang::try_fetch({
    tidyselect::eval_select(expr, data, parent.frame())
  },
  vctrs_error_subscript = function(cnd) {
    cnd$subscript_elt <- "element"
    cnd$call <- call
    rlang::cnd_signal(cnd)
  }
  )
}

