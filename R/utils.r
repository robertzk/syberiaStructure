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

stack <- methods::setRefClass('stack', list(elements = 'list'), methods = list(
  clear      = function()  { elements <<- list() },
  empty      = function()  { length(elements) == 0 },
  push       = function(x) { elements[[length(elements) + 1]] <<- x },
  peek       = function(n = 1)  {
    if (isTRUE(n)) return(elements)
    els <- seq(length(elements), length(elements) - n + 1)
    if (length(els) == 1) elements[[els]]
    else elements[els]
  },
  pop        = function()  {
    if (length(elements) == 0) stop("director:::stack is empty")
    tmp <- elements[[length(elements)]]
    elements[[length(elements)]] <<- NULL
    tmp
  },
  pop_all    = function()  { tmp <- elements; elements <<- list(); tmp }
))                                                                      

