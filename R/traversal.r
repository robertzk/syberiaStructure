# All functions related to traversal of file system for grabbing Syberia related files
#
# By convention, the structure from a syberia root project will look like this:
# 
# - data     # Data preparation for data sources coming from an external API
#   - sources
#     - data_source1
#       - data_source1.r
#       - helpers.r
#     - data_source2.r 
#   - test   # Unit tests for data preparation for data sources
# - models   # Files that parametrize the modeling process for each model version
#   - dev    # The development environment to be used as a sandbox
#   - prod   # Models will be copied over from dev to prod for deployment
#   - shared # Shared helpers for all models
#   - test   # Unit tests for the models - should be environment-agnostic (dev or prod)
# - bin      # Production scripts
# - syberia.config  # Configuration file for syberia project

#' Determine the root path of the current Syberia project.
#'
#' The root of the Syberia project is considered to be the directory containing
#' the \code{syberia.config} file.
#'
#' @param filename. If specified, it will attempt to find the Syberia root
#'   relative to the file name by traversing up its parent directories.
#'   If not given, Syberia will try to intelligently discern the current
#'   Syberia project by first looking at the cache for the previously used
#'   Syberia project and then by looking at the current directory. If no
#'   project is found, this function will return NULL.
#' @return see the \code{filename} parameter
syberia_root <- function(filename = NULL) {
  if (missing(filename)) {
    # If no filename was given, see if a syberia configuration was
    # given previously.
    return(
      if (!is.null(tmp <- get_cache('syberia_project'))) tmp
      else syberia_root(getwd())
    )
  }
  
  original_filename <- filename
  filename <- normalized_filename(filename)
  if (identical(filename, FALSE)) stop("Invalid syberia filename", call. = FALSE)
  filename <- normalizePath(filename)
  fileinfo <- file.info(filename)
  if (!fileinfo$isdir) filename <- dirname(filename)

  repeat {
    if (file.exists(tmp <- file.path(filename, 'syberia.config'))) {
      filename <- tmp
      break
    }
    prev_dir <- filename
    filename <- dirname(filename)
    if (filename == prev_dir)
      # Reached root of filesystem
      stop("No syberia project found relative to: ",
           original_filename, call. = FALSE)
  }
  filename
}

