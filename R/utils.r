`%||%` <- function(x, y) if (is.null(x)) y else x

#' Attempt to memoize a function using the memoise package.
#' 
#' This function will load the \code{memoise} package if it is
#' available, or do nothing otherwise.
#'
#' (Blame Hadley for the spelling of memoise.)
#'
#' @param fn function. The function to memoize.
#' @return nothing, but \code{try_memoize} will use non-standard
#'   evaluation to memoize in the calling environment.
#' @name try_memoize
try_memoize <- function(fn) {
  if ('memoise' %in% installed.packages()) {
    require(memoise)
    eval.parent(substitute(memoise(fn)))
  }
}

