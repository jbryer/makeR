\name{builder.rnw}
\alias{builder.rnw}
\title{This function will build Sweave (Rnw) files. Specifically this function will
first run Stangle, then Sweave, and finally texti2pdf.}
\usage{
  builder.rnw(project, theenv, fork = TRUE, ...)
}
\arguments{
  \item{project}{the project to be built.}

  \item{theenv}{the environment to build in.}

  \item{fork}{if true Sweave will be executed in a separate
  R instance.}

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

