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
#' @param error logical. If \code{TRUE}, it will return an error if the path
#'   is not found.
#' @export
#' @return see the \code{filename} parameter
syberia_root <- function(filename = NULL, error = FALSE) {
  if (missing(filename)) {
    # If no filename was given, see if a syberia configuration was
    # given previously.
    return(
      if (!is.null(tmp <- get_cache('syberia_project'))) tmp
      else syberia_root(getwd(), error = FALSE)
    )
  }
  
  original_filename <- filename
  # TODO: Windows support?
  if (!'/' %in% strsplit(filename, '')[[1]]) filename <- file.path('.', filename)
  filename <- suppressWarnings(normalizePath(filename))
  fileinfo <- file.info(filename)
  if (is.na(fileinfo$isdir) || !fileinfo$isdir) filename <- dirname(filename)

  repeat {
    if (file.exists(file.path(filename, 'syberia.config'))) break
    prev_dir <- filename
    filename <- suppressWarnings(normalizePath(dirname(filename)))
    if (filename == prev_dir)
      # Reached root of filesystem
      if (error)
        stop("No syberia project found relative to: ",
           original_filename, call. = FALSE)
      else return(NULL)
  }
  set_cache(filename, 'syberia_project')
  filename
}

#' Verifies that the given filename points to the base of a syberia project.
#'
#' @param filename character. The directory supposedly containing a syberia
#'   project.
#' @return TRUE or FALSE according as the directory is or is not a Syberia
#'   project (including if the directory does not exist).
is.syberia_project <- function(filename) {
  if (!is.character(filename)) FALSE
  else if (!tryCatch(file.exists(filename))) FALSE
  else syberia_root(filename) == normalizePath(filename)
}


#' Find all the model objects in a Syberia project.
#'
#' The convention is that model files have the same name
#' as the directory they are contained in (this allows other
#' files in the same directory to be used as helper files).
#' If no such file exists in a directory, all files are
#' assumed to be model files. For example, if we have
#'
#' default/en-US/model1.r
#' default/en-US/model2/model2.r
#' default/en-US/model2/helper.r
#'
#' only the first two will be considered to be model objects.
#' If there was a default/en-US/en-US.r, then the first would
#' no longer be considered a model object (it would be considered
#' a file with helper functions).
#'
#' @param env character. The syberia environment (e.g., \code{'dev'} or
#'   \code{'prod'}. The default is \code{c('dev', 'prod')}.
#' @param root character. The root of the syberia project. The default
#'   is \code{syberia_root()}.
#' @param by_mtime logical. Whether or not to sort the models in descending
#'   order by last modified time. The default is \code{TRUE}.
#' @seealso \code{\link{syberia_root}}
#' @export
#' @return a list of filenames containing syberia models
syberia_models <- function(env = c('dev', 'prod'), root = syberia_root(), by_mtime = TRUE) {
  stopifnot(is.syberia_project(root))
  path <- file.path(root, 'models', env)
  all_files <- unlist(lapply(seq_along(env), function(ix)
    file.path(env[[ix]],
      list.files(file.path(root, 'models', env[[ix]]), recursive = TRUE))
  ))

  # Find the models that have the same name as their parent directory
  dir_models <- grep('([^/]+)\\/\\1\\.r', all_files,
                     value = TRUE, ignore.case = TRUE)
  # Remove any model files in same directories as the dir_models
  lies_in_dir <- function(file, dir) substring(file, 1, nchar(dir)) == dir
  in_any_dir <- function(file, dirs) 
    any(vapply(dirs, lies_in_dir, logical(1), file = file))
  remaining_models <- vapply(all_files, Negate(in_any_dir), logical(1),
                             dirs = vapply(dir_models, dirname, character(1)))
  remaining_models <- all_files[remaining_models]

  models <- c(dir_models, remaining_models)
  if (identical(by_mtime, TRUE))
    models <- models[order(-sapply(file.path(root, 'models', models),
                                  function(f) file.info(f)$mtime))]

  models
}

