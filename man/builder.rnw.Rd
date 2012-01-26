\name{builder.rnw}
\alias{builder.rnw}
\title{This function will build Sweave (Rnw) files. Specifically this function will
first run Stangle, then Sweave, and finally texti2pdf.}
\usage{
  builder.rnw(sourceFile, theenv, ...)
}
\arguments{
  \item{sourceFile}{the source file to build. This may be
  NULL.}

  \item{theenv}{the environment to build in.}

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

