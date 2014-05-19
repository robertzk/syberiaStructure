#' Fetch info about a file relative to a syberia directory.
#'
#' Some additional information will be returned. Namely, a list with
#' four keys, `current`, `cached`, `value`, and `modified`.
#' The former two will contain information relating to the current
#' and previous execution of this file, while `value` contains
#' a function that will execute the file and return its output,
#' and `modified` is a logical flag indicating whether or not the
#' file has been modified since it was last executed by Syberia.
#' 
#' Internally, the cached information about the file is stored in the
#' syberia project's registry under the key \code{resources/resource_cache}.
#'
#' @name syberia_resource
#' @param filename character. The filename of the R script to execute
#'   relative to a syberia project.
#' @param root character. The root of the syberia project in which to
#'   look for \code{filename}. The default is \code{syberia_root()}.
#' @param provides list or environment. A list or environment of values
#'   to provide to the file.
#' @param ... additional arguments to pass to the \code{base::source}
#'   function that gets executed when the `value` is accessed.
#' @return a four-element list with names `current`, `cached`, `value`,
#'   and `modified`. The former two will both be two-element lists containing
#'   keys `info` and `body` (unless Syberia has never executed the file before
#'   in which case the latter will be \code{NULL}). The `info` key holds the
#'   result of calling R's built-in \code{file.info} on the file (and includes
#'   information like created at and modified at time), whereas the `body`
#'   key holds a character representation of the body of the file (useful
#'   for comparing if any actual modifications have been made).
#'
#'   The `value` key in the returned list holds a function which will
#'   execute the given file in the \code{provides} environment. A function
#'   is returned instead of the actual value to let the caller control
#'   whether or not the file is executed (e.g., based on its properties
#'   in the `current` list). Most frequently, this means consulting the
#'   `modified` key of the returned list, which will hold a logical
#'   indicating whether or not the file has been modified since last
#'   executed by Syberia (if this was never the case, `modified` will
#'   be \code{FALSE}).
syberia_resource <- function(filename, root = syberia_root(), provides = list(), ...) {
  resource_cache <- .get_registry_key('resource/resource_cache', root)
  resource_key <- function(filename, root) # Given a/b/c/d and a/b, extracts c/d
    substring(tmp <- normalizePath(filename), nchar(normalizePath(root)) + 1, nchar(tmp))
  resource_key <- resource_key(filename, root)
  cache_details <- resource_cache[[resource_key]]

  if (!is.environment(provides)) {
    provides <- if (length(provides) == 0) new.env() else as.environment(provides)
    parent.env(provides) <- get_cache('runtime/current_env') %||% new.env()
  }

  if (!file.exists(filename))
    stop("Syberia resource '", filename, "' in syberia project '", root,
         "' does not exist.", call. = FALSE)

  resource_info <- file.info(filename)
  resource_body <- paste(readLines(filename), collapse = "\n")

  current_details <- list(
    info = resource_info,
    body = resource_body
  )

  resource_cache[[resource_key]] <- current_details
  .set_registry_key('resource/resource_cache', resource_cache, root)
  # TODO: (RK) For large syberia projects, maybe this should dynamically
  # switch to tracking resources using the file system rather than one big list.

  source_args <- append(list(filename, local = provides), list(...))
  value <- function() do.call(base::source, source_args)$value
  modified <- resource_info$mtime > cache_details$info$mtime %||% 0

  list(current = current_details, cached = cache_details,
       value = value, modified = modified)
}

# TODO: (RK) It would be cool to dynamically look at the memory size of the object
# in the result, and store it in the cache as well if it's small enough and the
# file has not been modified.
