\name{builder.rnw.native}
\alias{builder.rnw.native}
\title{Build Sweave files using native calls to pdflatex.}
\usage{
  builder.rnw.native(project, theenv, fork = TRUE,
    debug = TRUE, ...)
}
\arguments{
  \item{project}{the project to be built.}

  \item{theenv}{the environment to build in.}

  \item{fork}{if true Sweave will be executed in a separate
  R instance.}

  \item{debug}{debug option sent to the Sweave function. If
  true, the output of R code from the Rnw file will be
  printed as it is running.}

  \item{native}{if not NULL, the shell command to execute
  to generate}

  \item{...}{other unspecified parameters}
}
\value{
  the name of the file if successfully built.
}
\description{
  This function will build Sweave (Rnw) files. This
  function is similar to \code{\link{builder.rnw}} but
  instead of calling \code{texti2pdf}, this will call latex
  natively instead of using the \code{texi2dvi} function.
  \code{ system('pdflatex FILENAME') system('bibtex
  FILENAME') system('pdflatex FILENAME') system('pdflatex
  FILENAME') }
}
\seealso{
  \code{\link{system}}
}

