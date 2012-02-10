\name{builder.rnw}
\alias{builder.rnw}
\title{This function will build Sweave (Rnw) files. Specifically this function will
first run Stangle, then Sweave, and finally texti2pdf.}
\usage{
  builder.rnw(project, theenv, fork = TRUE, debug = TRUE,
    ...)
}
\arguments{
  \item{project}{the project to be built.}

  \item{theenv}{the environment to build in.}

  \item{fork}{if true Sweave will be executed in a separate
  R instance.}

  \item{debug}{debug option sent to the Sweave function. If
  true, the output of R code from the Rnw file will be
  printed as it is running.}

  \item{...}{other unspecified parameters}
}
\value{
  the name of the file if successfully built.
}
\description{
  This function will build Sweave (Rnw) files. Specifically
  this function will first run Stangle, then Sweave, and
  finally texti2pdf.
}

