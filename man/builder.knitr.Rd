\name{builder.knitr}
\alias{builder.knitr}
\title{This function will documents using \code{knitr}. Since \code{knitr} is flexible
enough to support multiple file types (e.g. Sweave, HTML, Markdown, etc.), specifying
\code{Project$SourceFile} and \code{output} (see \code{\link[knitr:knit]{knit}})
is necessary for file types other than Sweave (i.e. .rnw files).}
\usage{
  builder.knitr(project, theenv, ...)
}
\arguments{
  \item{project}{the project to be built.}

  \item{theenv}{the environment to build in.}

  \item{...}{other parameters passed to
  \code{\link[knitr]{knit}} and
  \code{\link[knitr:knit]{curl}}}
}
\value{
  the name of the file if successfully built.
}
\description{
  This function will documents using \code{knitr}. Since
  \code{knitr} is flexible enough to support multiple file
  types (e.g. Sweave, HTML, Markdown, etc.), specifying
  \code{Project$SourceFile} and \code{output} (see
  \code{\link[knitr:knit]{knit}}) is necessary for file
  types other than Sweave (i.e. .rnw files).
}

