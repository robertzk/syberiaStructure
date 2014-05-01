\name{syberia_models}
\alias{syberia_models}
\title{Find all the model objects in a Syberia project.}
\usage{
syberia_models(env = c("dev", "prod"), root = syberia_root(),
  by_mtime = TRUE)
}
\arguments{
  \item{env}{character. The syberia environment (e.g.,
  \code{'dev'} or \code{'prod'}. The default is
  \code{c('dev', 'prod')}.}

  \item{root}{character. The root of the syberia project.
  The default is \code{syberia_root()}.}

  \item{by_mtime}{logical. Whether or not to sort the
  models in descending order by last modified time. The
  default is \code{TRUE}.}
}
\value{
a list of filenames containing syberia models
}
\description{
The convention is that model files have the same name as
the directory they are contained in (this allows other
files in the same directory to be used as helper files). If
no such file exists in a directory, all files are assumed
to be model files. For example, if we have
}
\details{
default/en-US/model1.r default/en-US/model2/model2.r
default/en-US/model2/helper.r

only the first two will be considered to be model objects.
If there was a default/en-US/en-US.r, then the first would
no longer be considered a model object (it would be
considered a file with helper functions).
}
\seealso{
\code{\link{syberia_root}}
}

