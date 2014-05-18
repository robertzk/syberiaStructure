#' Execute a file relative to a syberia directory.
#'
#' Some additional information will be returned. Namely, a list with
#' two keys, `current` and `cached`. The former will contain information
#' relating to the current execution of this file, while the latter
#' contains information relating to the previous execution of this file.
#' The ultimate use of this information depends on the caller, but potential
#' applications include determining whether the contents of the file changed
#' or whether the file has been modified on the file system.
#' 
#' Internally, the cached information about the file is stored in the
#' syberia project's registry under the key \code{resources/resource_cache}.
#'
#' @param filename character. The filename of the R script to execute
#'   relative to a syberia project.
#' @param root character. The root of the syberia project in which to
#'   look for \code{filename}. The default is \code{syberia_root()}.
#' @param provides list or environment. A list or environment of values
#'   to provide to the file.
#' @param execute logical or character. Whether or not to execute \code{filename}.
#'   The default is \code{TRUE}, but if set to \code{FALSE}, no
#'   element with key `value` will be returned in the `current` list.
#'   Additionally, the character value "onchange" can be specified which
#'   will execute the file if and only if its last modified time has change
#'   since syberia's last attempt to access it.
#' @return a two-element list with names `current` and `cached`, respectively.
#'   Both will be lists containing keys `info` and `body`, but only the former
#'   will contain `value`, the result of executing the R file specified
#'   by the \code{filename} parameter. The `info` key holds the result of
#'   of calling R's built-in \code{file.info} on the file (and includes
#'   information like created at and modified at time), whereas the `body`
#'   key holds a character representation of the body of the file (useful
#'   for comparing if any actual modifications have been made).
syberia_resource <- function(filename, root = syberia_root(), provides = list(),
                             execute = TRUE) {
  resource_cache <- .get_registry_key('resource/resource_cache', root)
  resource_key <- function(filename, root) # Given a/b/c/d and a/b, extracts c/d
    substring(tmp <- normalizePath(filename), nchar(normalizePath(root)) + 1, nchar(tmp))
  resource_key <- resource_key(filename, root)
  cache_details <- resource_cache[[resource_key]]

  if (!is.environment(provides)) provides <- as.environment(provides)
  if (!file.exists(filename))
    stop("Syberia resource '", filename, "' in syberia project '", root,
         "' does not exist.", call. = FALSE)

  resource_info <- file.info(filename)
  resource_body <- paste(readLines(filename), collapse = "\n")
  result <-
    if (identical(execute, TRUE) ||
         (identical(execute, "onchange") &&
          resource_info$mtime > cache_details$info$mtime))
      source(filename, local = provides)$value

  current_details <- list(
    info = resource_info,
    body = resource_body
  )
  if (!is.null(result)) current_details$value <- result

  resource_cache[[resource_ey]] <- 
    current_details[-which(names(current_output) == 'value')]
  .set_registry_key('resource/resources_cache', resource_cache, root)
  # TODO: (RK) For large syberia projects, maybe this should dynamically
  # switch to tracking resources using the file system rather than one big list.

  list(current = current_details, cached = cached_details)
}

# TODO: (RK) It would be cool to dynamically look at the memory size of the object
# in the result, and store it in the cache as well if it's small enough and the
# file has not been modified.
